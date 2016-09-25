%% Copyright (c) 2014-2016 Ulf Leopold.
%%
%% The load balancer is configured with a pool of fuses. It will
%% dispatch requests via them using the selected algorithm. If a fuse
%% burns it will be removed from the pool. Once the fuse is active again
%% it will notify the LB and be included in the pool. Different load
%% balancing algorithms can be used such as round_robin or prio.
-module(fuse_lb).
-behaviour(gen_server).

%% API.
-export([start_link/2, start_link/3, call/2, num_fuses_active/1, stop/1]).

%% Gen server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Algorithm is the load balancing algorithm to use. Available is the
%% fuses that are not burnt.
-record(state, {algorithm=none, available=[], log=none}).

%%%_* API ==============================================================
-spec start_link([{any(), fuse:timeout_entry(), fun()}],
                 atom()) -> ignore | {error, _} | {ok, pid()}.
start_link(FuseData, Algorithm) ->
  gen_server:start_link(?MODULE, [FuseData, Algorithm], []).

-spec start_link([{any(), fuse:timeout_entry(), fun()}],
                 atom(), fun()) -> ignore | {error, _} | {ok, pid()}.
start_link(FuseData, Algorithm, Log) ->
  gen_server:start_link(?MODULE, [FuseData, Algorithm, Log], []).

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
init([FuseData, Algorithm]) -> init(FuseData, Algorithm, fun(_, _) -> ok end);
init([FuseData, Algorithm, LogFun]) -> init(FuseData, Algorithm, LogFun).

init(FuseData, Algorithm, LogFun) ->
  Fuses0 = lists:map(fun({Init, Tmos, Probe}) ->
                         {ok, Fuse} = fuse:start_link(Init, Tmos, Probe,
                                                      self(), LogFun),
                         Fuse
                     end, FuseData),
  Fuses = initial_sort(Algorithm, Fuses0),
  {ok, #state{algorithm=Algorithm, available=Fuses, log=LogFun}}.

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
initial_sort(round_robin, L) -> L;
initial_sort(prio, L)        -> lists:sort(L).

pick(_, [])                -> {no_fuses_left, []};
pick(round_robin, [H | T]) -> {H, T ++ [H]};
pick(prio, [H | _T] = L)   -> {H, L}.

add_back(round_robin, F, Available) -> Available ++ [F];
add_back(prio, F, Available) -> lists:sort([F | Available]).
