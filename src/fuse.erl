%% Copyright (c) 2014 Ulf Leopold.
%%
%% A fuse is implemented as a process. It is created using:
%%
%%    fuse:start_link(UserData, Timeouts, Probe, Owner).
%%
%% A fuse can have two states: active or burnt. Using fuse:call(Fuse,
%% Fun) a given Fun can be executed using the provided Fuse. If the Fuse
%% is burnt, then Fun won't be called and {error, fuse_burnt} will
%% instead be returned. If the Fuse is active, then Fun will be called
%% with UserData as its single argument. Fun must either return:
%% {available, <return data>} or {unavailable, <return data>} depending
%% on the availability of the service that Fun relies on. In either case
%% <return data> is returned to the caller. If "available" was returned,
%% then the Fuse remains available. If "unavailable" was returned, then
%% the Fuse changes to state burnt.
%%
%% A Fuse that is in state burnt will try to recover by periodically
%% calling the provided Probe function. If the Probe returns {available,
%% UserData} the Fuse will change back to active state (and notify its
%% Owner via a "re_fuse" message). If the Probe returns {unavailable,
%% UserData} then it will remain unavailable. How often the Probe is
%% called is specified by the Timeouts argument. It can be used to
%% implement back-off.
-module(fuse).
-behaviour(gen_server).

%% API.
-export([start_link/5, call/2, is_burnt/1, stop/1]).

%% Gen server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export_type([timeout_entry/0]).

-record(state, {data=false, burnt=false, timeouts=[], start_timeouts=[],
                probe=none, owner=none, ignore_exits_pids=[],
                timeout_ref=none, log=none}).

-type timeout_entry() :: timeout_count() | timeout_val().
-type timeout_val()   :: integer().
-type timeout_count() :: {integer(), integer()}.

%%%_* API ==============================================================
-spec start_link(any(), [timeout_entry()], fun(), pid(), fun()) ->
                    ignore | {error, _} | {ok, pid()}.
start_link(Init, Timeouts, Probe, Owner, LogFun) ->
  gen_server:start_link(?MODULE, [Init, Timeouts, Probe, Owner, LogFun], []).

-spec call(pid(), fun()) -> {available, _} | {unavailable, _} |
                            {error, fuse_burnt}.
call(Fuse, Fun) ->
  {ok, {Burnt, UserData}} = gen_server:call(Fuse, check_burnt),
  case Burnt of
    false ->
      case Fun(UserData) of
        {available, _}   = Val -> Val;
        {unavailable, _} = Val -> gen_server:cast(Fuse, burn), Val
      end;
    true -> {error, fuse_burnt}
  end.

-spec is_burnt(pid()) -> boolean().
is_burnt(Fuse) ->
  {ok, {Burnt, _}} = gen_server:call(Fuse, check_burnt),
  Burnt.

-spec stop(pid()) -> any().
stop(Fuse) -> gen_server:call(Fuse, stop).

%%%_* Gen server callbacks =============================================
init([Init, Timeouts, Probe, Owner, LogFun]) ->
  process_flag(trap_exit, true),
  UserData = case is_function(Init) of
               true  -> Init();
               false -> Init
             end,
  {ok, #state{data=UserData, timeouts=Timeouts, start_timeouts=Timeouts,
              probe=Probe, owner=Owner, log=LogFun}}.

handle_call(check_burnt, _From, #state{data=UserData, burnt=Burnt} = State) ->
  {reply, {ok, {Burnt, UserData}}, State};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(burn, #state{burnt=true} = State) -> {noreply, State};
handle_cast(burn, #state{burnt=false} = State) ->
  {Tmo, State1} = update_timeouts(State),
  erlang:send_after(Tmo, self(), {timeout, TmoRef = make_ref()}),
  {noreply, State1#state{burnt=true, timeout_ref=TmoRef}}.

handle_info({'EXIT', Pid, _}, #state{owner=Owner} = State) ->
  true = Pid =/= Owner,
  {noreply, State};
handle_info({timeout, TmoRef}, #state{burnt=true,
                                      timeout_ref=TmoRef} = State) ->
  {noreply, probe(State)};
handle_info(Msg, #state{log=L} = State)  ->
  L("fuse: Received unknown message (message=~p, pid=~p, state=~p).",
    [Msg, self(), State]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%_* Helpers ==========================================================
update_timeouts(#state{timeouts=[{_Tmo, 0} | T]} = State) ->
  update_timeouts(State#state{timeouts=T});
update_timeouts(#state{timeouts=[{Tmo, Cnt} | T]} = State) ->
  {Tmo, State#state{timeouts=[{Tmo, Cnt - 1} | T]}};
update_timeouts(#state{timeouts=[Tmo]} = State) ->
  {Tmo, State}.

reset_timeouts(State) -> State#state{timeouts=State#state.start_timeouts}.

notify_owner(State) -> gen_server:cast(State#state.owner, {re_fuse, self()}).

probe(State) ->
  Probe = State#state.probe,
  try Probe(State#state.data) of
      {available, Data}                        ->
        probe_succeeded(State#state{data=Data});
      {unavailable, Data}                      ->
        probe_failed(State#state{data=Data})
  catch
    _:_ -> probe_failed(State)
  end.

probe_succeeded(State) ->
  notify_owner(State),
  reset_timeouts(State#state{burnt=false}).

probe_failed(State) ->
  {Tmo, State1} = update_timeouts(State),
  erlang:send_after(Tmo, self(), {timeout, State1#state.timeout_ref}),
  State1.

%%%_* Eunit ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tmo_test() ->
  ?assertEqual({50, #state{timeouts=[{50, 9}]}},
               update_timeouts(#state{timeouts=[{50, 10}]})),

  ?assertEqual({50, #state{timeouts=[{50, 0}]}},
               update_timeouts(#state{timeouts=[{50, 1}]})),

  ?assertEqual({500, #state{timeouts=[{500, 1}]}},
               update_timeouts(#state{timeouts=[{50, 0}, {500, 2}]})),

  ?assertEqual({1000, #state{timeouts=[1000]}},
               update_timeouts(#state{timeouts=[{500, 0}, 1000]})),

  ?assertEqual({1000, #state{timeouts=[1000]}},
               update_timeouts(#state{timeouts=[1000]})).

reset_timeouts_test() ->
  T = [{500, 10}, 1000],
  ?assertEqual(#state{timeouts=T, start_timeouts=T},
               reset_timeouts(#state{timeouts=[1000], start_timeouts=T})).

-endif.
