%% Copyright (c) 2014 Ulf Leopold.
-module(fuse_pool).
-behaviour(gen_server).

%% API.
-export([start_link/1, start_link/2, call/2, available_fuses/1,
         queued_work/1, stop/1]).

%% Gen server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {available=[], queue=queue:new(), log=none}).

%%%_* API ==============================================================
-spec start_link([{any(), fuse:timeout_entry(), fun()}]) ->
                    ignore | {error, _} | {ok, pid()}.
start_link(PoolData) -> gen_server:start_link(?MODULE, [PoolData], []).

-spec start_link([{any(), fuse:timeout_entry(), fun()}],
                 fun()) -> ignore | {error, _} | {ok, pid()}.
start_link(PoolData, Log) ->
  gen_server:start_link(?MODULE, [PoolData, Log], []).

-spec call(pid(), fun()) -> any().
call(Pool, Fun) -> gen_server:call(Pool, {do_work, Fun}).

-spec available_fuses(pid()) -> integer().
available_fuses(Pool) ->
  {ok, N} = gen_server:call(Pool, get_num_available),
  N.

-spec queued_work(pid()) -> integer().
queued_work(Pool) ->
  {ok, N} = gen_server:call(Pool, get_num_queued),
  N.

-spec stop(pid()) -> any().
stop(Lb) -> gen_server:call(Lb, stop).

%%%_* Gen server callbacks =============================================
init([PoolData]) -> init(PoolData, fun(_, _) -> ok end);
init([PoolData, LogFun]) -> init(PoolData, LogFun).

init(PoolData, LogFun) ->
  A = lists:map(fun({UserData, Tmos, Probe}) ->
                    {ok, Fuse} = fuse:start_link(UserData, Tmos, Probe, self()),
                    Fuse
                end, PoolData),
  {ok, #state{available=A, log=LogFun}}.

handle_call({do_work, Fun}, From, #state{available=[F | T], log=Log} = S) ->
  spawn_work(From, F, Fun, Log),
  {noreply, S#state{available=T}};
handle_call({do_work, Fun}, From, #state{available=[], queue=Q} = S) ->
  {noreply, S#state{queue=queue:in({From, Fun}, Q)}};
handle_call(get_num_available, _From, #state{available=A} = S) ->
  {reply, {ok, length(A)}, S};
handle_call(get_num_queued, _From, #state{queue=Q} = S) ->
  {reply, {ok, queue:len(Q)}, S};
handle_call(stop, _From, S) ->
  {stop, normal, ok, S}.

handle_cast({available, F}, #state{} = S)  ->
  {noreply, add_back_fuse(F, S)};
handle_cast({re_fuse, F}, #state{log=L} = S) ->
  L("fuse_pool: Adding refreshed fuse (pid=~p) back to pool.", [F]),
  {noreply, add_back_fuse(F, S)};
handle_cast(Msg, S) -> {stop, {unexpected_cast, Msg}, S}.

handle_info(Msg, S) -> {stop, {unexpected_info, Msg}, S}.

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
            gen_server:reply(From, X);
          {unavailable, X} ->
            Log("fuse_pool: Fuse (pid=~p) burnt, removing from pool.",
                [Fuse]),
            gen_server:reply(From, X);
          {error, fuse_burnt} ->
            error(fuse_burnt)
        end
    end).

add_back_fuse(Fuse, #state{available=A, queue=Q0, log=Log} = S) ->
  case queue:is_empty(Q0) of
    true  ->
      S#state{available=[Fuse | A]};
    false ->
      {{value, {From, Fun}}, Q} = queue:out(Q0),
      spawn_work(From, Fuse, Fun, Log),
      S#state{available=A, queue=Q}
  end.
