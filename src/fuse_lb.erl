%% Copyright (c) 2014-2016 Ulf Leopold.
%%
%% Fuse_lb is configured with a set of fuses. It will dispatch work
%% requests via them using the selected algorithm. If a fuse burns it
%% will be removed from the set. Once the fuse is mended it will again
%% be included in the set. Different load balancing algorithms can be
%% used such as round_robin or prio. Fuse_lb does not impose a limit on
%% the number of simultaneous ongoing requests per fuse. If you need
%% such a limit, then instead use fuse_pool.
%%
%% When initializing a fuse_lb a [fuse:fuse_data()] list is provided
%% with config for the respective fuse. It contains the fuse user data,
%% probe back-off schedule, and the probe function. Fuses start out in a
%% burnt state which means that they will call the probe function to
%% initialize themselves.
-module(fuse_lb).
-behaviour(gen_server).

-export([start_link/2, start_link/3, call/2, num_fuses_active/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% 'algorithm' is the load balancing algorithm to use. 'available' is the
%% fuses that are not burnt. 'log' is the log fun.
-record(state, {algorithm=none, available=[], log=none}).

%%%_* API ==============================================================
-spec start_link([fuse:fuse_data()],
                 atom()) -> ignore | {error, _} | {ok, pid()}.
start_link(FusesConfig, Algorithm) ->
  gen_server:start_link(?MODULE, [FusesConfig, Algorithm], []).

-spec start_link([fuse:fuse_data()],
                 atom(), fun()) -> ignore | {error, _} | {ok, pid()}.
start_link(FusesConfig, Algorithm, Log) ->
  gen_server:start_link(?MODULE, [FusesConfig, Algorithm, Log], []).

-spec call(pid() | atom(), fun()) -> {ok, any()} | {error, fuse_burnt} |
                                     {error, no_fuses_left}.
call(Lb, Fun) ->
  {ok, Fuse} = gen_server:call(Lb, get_fuse),
  case Fuse of
    no_fuses_left -> {error, no_fuses_left};
    _ ->
      case fuse:call(Fuse, Fun) of
        {available, X}   -> {ok, X};
        {unavailable, X} -> gen_server:cast(Lb, {burnt_fuse, Fuse}), {ok, X};
        {error, fuse_burnt} = E -> E
      end
  end.

-spec num_fuses_active(pid() | atom()) -> integer().
num_fuses_active(Lb) -> gen_server:call(Lb, num_fuses_active).

-spec stop(pid() | atom()) -> any().
stop(Lb) -> gen_server:call(Lb, stop).

%%%_* Gen server callbacks =============================================
init([FusesConfig, Algorithm]) ->
  init(FusesConfig, Algorithm, fun(_, _) -> ok end);
init([FusesConfig, Algorithm, LogFun]) ->
  init(FusesConfig, Algorithm, LogFun).

init(FusesConfig, Algorithm, LogFun) ->
  lists:map(fun(FuseData) ->
                {ok, Fuse} = fuse:start_link(FuseData, self(), LogFun),
                Fuse
            end, FusesConfig),
  {ok, #state{algorithm=Algorithm, available=[], log=LogFun}}.

handle_call(get_fuse, _From, #state{algorithm=Algorithm,
                                    available=Available0} = S) ->
  {Fuse, Available} = pick(Algorithm, Available0),
  {reply, {ok, Fuse}, S#state{algorithm=Algorithm, available=Available}};
handle_call(num_fuses_active, _From, #state{available=Available} = S) ->
  {reply, length(Available), S};
handle_call(stop, _From, S) ->
  {stop, normal, ok, S}.

handle_cast({burnt_fuse, F}, #state{available=Available, log=L} = S) ->
  L("fuse_lb: Fuse (pid=~p) burnt, removing from pool.", [F]),
  {noreply, S#state{available=Available -- [F]}};
handle_cast({re_fuse, F}, #state{algorithm=Algorithm, available=Available,
                                 log=L} = S) ->
  L("fuse_lb: Adding refreshed fuse (pid=~p) back to pool.", [F]),
  {noreply, S#state{available=add_back(Algorithm, F, Available)}};
handle_cast(stop, S) -> {stop, ok, S};
handle_cast(Msg, S)  -> {stop, {unexpected_cast, Msg}, S}.

handle_info(Msg, S) -> {stop, {unexpected_info, Msg}, S}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%_* Internal =========================================================
pick(_, [])                -> {no_fuses_left, []};
pick(round_robin, [H | T]) -> {H, T ++ [H]};
pick(prio, [H | _T] = L)   -> {H, L}.

add_back(_, F, Available) -> lists:sort([F | Available]).
