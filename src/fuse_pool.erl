%% Copyright (c) 2014-2016 Ulf Leopold.
%%
%% Similarly to fuse_lb, fuse_pool is configured with a set of fuses and
%% it will dispatch work requests via them. Fuse_pool will only allow a
%% single outstanding work request per fuse. If all fuses are busy, then
%% work requests will wait (up until 'QueueTmo' milliseconds) until
%% a fuse becomes available and the work request can be handled.
%%
%% When initializing a fuse_pool a [fuse:fuse_data()] list is provided
%% with config for the respective fuse. It contains the fuse user data,
%% probe back-off schedule, and the probe function. Fuses start out in a
%% burnt state which means that they will call the probe function to
%% initialize themselves.
-module(fuse_pool).
-behaviour(gen_server).

-export([start_link/2, start_link/3, call/2, num_fuses_active/1,
         num_workers_idle/1, num_jobs_queued/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% How often the queue is pruned.
-define(PRUNE_PERIOD, 1000).

%% 'available' is the fuses that are not burnt and not in use. 'queue'
%% is jobs waiting for an available fuse. 'tmo' is the max amount of
%% milliseconds a job should be allowed in the queue. 'log' is the log
%% fun. 'worker_shortage' is set to true if queuing has been started.
-record(state, {all=[], available=[], queue=queue:new(), tmo=undefined,
                log=undefined, worker_shortage=false}).

%%%_* API ==============================================================
-spec start_link([fuse:fuse_data()], integer()) ->
                    ignore | {error, _} | {ok, pid()}.
start_link(FusesConfig, QueueTmo) ->
  gen_server:start_link(?MODULE, [FusesConfig, QueueTmo], []).

-spec start_link([fuse:fuse_data()], integer(),
                 fun()) -> ignore | {error, _} | {ok, pid()}.
start_link(FusesConfig, QueueTmo, Log) ->
  gen_server:start_link(?MODULE, [FusesConfig, QueueTmo, Log], []).

-spec call(pid() | atom(), fun()) -> {ok, any()} | {error, fuse_pool_queue_tmo}.
call(Pool, Fun) -> gen_server:call(Pool, {do_work, Fun}, infinity).

-spec num_fuses_active(pid() | atom()) -> integer().
num_fuses_active(Pool) ->
  L0 = gen_server:call(Pool, get_all),
  L1 = lists:filter(fun(X) -> not fuse:is_burnt(X) end, L0),
  length(L1).

-spec num_workers_idle(pid() | atom()) -> integer().
num_workers_idle(Pool) -> gen_server:call(Pool, get_num_available).

-spec num_jobs_queued(pid() | atom()) -> integer().
num_jobs_queued(Pool) -> gen_server:call(Pool, get_num_queued).

-spec stop(pid() | atom()) -> any().
stop(Lb) -> gen_server:call(Lb, stop).

%%%_* Gen server callbacks =============================================
init([FusesConfig, QueueTmo]) ->
  init(FusesConfig, QueueTmo,  fun(_, _) -> ok end);
init([FusesConfig, QueueTmo, LogFun]) ->
  init(FusesConfig, QueueTmo, LogFun).

init(FusesConfig, QueueTmo, LogFun) ->
  A = lists:map(fun(FuseData) ->
                    {ok, Fuse} = fuse:start_link(FuseData, self(), LogFun),
                    Fuse
                end, FusesConfig),
  erlang:send_after(?PRUNE_PERIOD, self(), prune),
  {ok, #state{all=A, available=[], tmo=QueueTmo, log=LogFun}}.

handle_call({do_work, Fun}, From, #state{available=[F | T], log=Log} = S) ->
  spawn_work(From, F, Fun, Log),
  {noreply, S#state{available=T, worker_shortage=false}};
handle_call({do_work, Fun}, From, #state{available=[], queue=Q} = S) ->
  log_worker_shortage(S),
  NewQ = queue:in({From, Fun, now()}, Q),
  {noreply, S#state{queue=NewQ, worker_shortage=true}};
handle_call(get_all, _From, #state{all=All} = S) ->
  {reply, All, S};
handle_call(get_num_available, _From, #state{available=Available} = S) ->
  {reply, length(Available), S};
handle_call(get_num_queued, _From, #state{queue=Q} = S) ->
  {reply, queue:len(Q), S};
handle_call(stop, _From, S) ->
  {stop, normal, ok, S}.

handle_cast({available, F}, #state{} = S)  ->
  {noreply, add_back_fuse(F, S)};
handle_cast({fuse_burnt, _F}, #state{} = S) ->
  {noreply, S};
handle_cast({fuse_mended, F}, #state{log=L} = S) ->
  L("fuse_pool: Adding refreshed fuse (pid=~p) back to pool.", [F]),
  {noreply, add_back_fuse(F, S)};
handle_cast(Msg, S) -> {stop, {unexpected_cast, Msg}, S}.

handle_info(prune, #state{queue=Q0, tmo=QueueTmo} = S) ->
  Prune = fun({From, _Fun, Ts}) ->
              case timer:now_diff(now(), Ts)  > (QueueTmo * 1000) of
                true  ->
                  gen_server:reply(From, {error, fuse_pool_queue_tmo}),
                  false;
                false ->
                  true
              end
          end,
  Q = queue:filter(Prune, Q0),
  erlang:send_after(?PRUNE_PERIOD, self(), prune),
  {noreply, S#state{queue=Q}};
handle_info(Msg, S) ->
  {stop, {unexpected_info, Msg}, S}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%_* Internal =========================================================
spawn_work(From, Fuse, Fun, Log) ->
  Pid = self(),
  spawn_link(
    fun() ->
        case fuse:call(Fuse, Fun) of
          {available, X} ->
            gen_server:cast(Pid, {available, Fuse}),
            gen_server:reply(From, {ok, X});
          {unavailable, X} ->
            Log("fuse_pool: Fuse (pid=~p) burnt, removing from pool.",
                [Fuse]),
            gen_server:reply(From, {ok, X});
          {error, fuse_burnt} ->
            error(fuse_burnt)
        end
    end).

add_back_fuse(Fuse, #state{available=A, queue=Q0, log=Log} = S) ->
  case queue:is_empty(Q0) of
    true  ->
      S#state{available=lists:usort([Fuse | A])};
    false ->
      {{value, {From, Fun, _Ts}}, Q} = queue:out(Q0),
      spawn_work(From, Fuse, Fun, Log),
      S#state{available=A, queue=Q}
  end.

log_worker_shortage(#state{worker_shortage=false, log=Log}) ->
  Log("fuse_pool: No fuse available. Operation queueing started.", []);
log_worker_shortage(_State)                                   ->
  ok.
