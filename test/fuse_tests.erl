%% Copyright (c) 2014-2016 Ulf Leopold.
-module(fuse_tests).

-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ============================================================

probe_available(state_data) ->
  timer:sleep(random:uniform(4)),
  {available, state_data}.

give_time_to_initialize_fuses() -> timer:sleep(7).

call_not_burnt_test() ->
  {ok, F} = fuse:start_link({state_data, [500], fun probe_available/1},
                            self(), fun(_, _) -> ok end),
  give_time_to_initialize_fuses(),
  ?assertEqual({available, value}, fuse:call(F, fun(state_data) ->
                                                    {available, value}
                                                end)),
  fuse:stop(F).

call_burnt_test() ->
  {ok, F} = fuse:start_link({state_data, [500], fun probe_available/1},
                            self(), fun(_, _) -> ok end),
  give_time_to_initialize_fuses(),
  ?assertEqual({unavailable, someval}, fuse:call(F, fun(state_data) ->
                                                        {unavailable, someval}
                                                    end)),
  ?assertEqual({error, fuse_burnt}, fuse:call(F, fun(state_data) ->
                                                     error(should_not_be_called)
                                                 end)),
  timer:sleep(600),
  ?assertEqual({available, value}, fuse:call(F, fun(state_data) ->
                                                    {available, value}
                                                end)),
  fuse:stop(F).

call_burnt_multiple_test() ->
  Cnt = fuse_misc:make_counter(),
  {ok, F} = fuse:start_link({init_state_data, [{1, 10}, {10, 10}, 50],
                             fun(init_state_data) -> {available, state_data};
                                (state_data)      ->
                                 case Cnt(inc) of
                                   21 -> {available, state_data};
                                   _  -> {unavailable, state_data}
                                 end
                             end}, self(), fun(_, _) -> ok end),
  give_time_to_initialize_fuses(),
  ?assertEqual({unavailable, someval}, fuse:call(F, fun(state_data) ->
                                                        {unavailable, someval}
                                                    end)),
  ?assertEqual({error, fuse_burnt}, fuse:call(F, fun(state_data) ->
                                                     error(should_not_be_called)
                                                 end)),
  timer:sleep(200),
  ?assertEqual(22, Cnt(get)),
  ?assertEqual({available, value}, fuse:call(F, fun(state_data) ->
                                                    {available, value}
                                                end)),
  receive
    {_, {fuse_burnt, F}} -> ok
  end,
  receive
    {_, {fuse_mended, F}} -> ok
  end,
  fuse:stop(F).

update_state_test() ->
  Probe = fun(init_data)  -> {available, state_data};
               (state_data) -> {available, new_state}
            end,
  {ok, F} = fuse:start_link({init_data, [50], Probe},
                            self(), fun(_, _) -> ok end),
  give_time_to_initialize_fuses(),
  ?assertEqual({unavailable, someval}, fuse:call(F, fun(state_data) ->
                                                        {unavailable, someval}
                                                    end)),
  ?assertEqual({error, fuse_burnt}, fuse:call(F, fun(state_data) ->
                                                     error(should_not_be_called)
                                                 end)),
  timer:sleep(100),
  ?assertEqual({available, someval}, fuse:call(F, fun(new_state) ->
                                                      {available, someval}
                                                  end)),
  receive
    {_, {fuse_burnt, F}} -> ok
  end,
  receive
    {_, {fuse_mended, F}} -> ok
  end,
  fuse:stop(F).

%% Prove that fuse can do at least 10,000 dispatches in a second and
%% that it is less than 4 times as expensive as calling a fun.
%% perf_test() ->
%%   N = 10000,
%%   {ok, F} = fuse:start_link(state_data, [{1, 10}, {10, 10}, 50],
%%                             fun(state_data) -> {available, state_data} end, self()),
%%   Fun = fun(state_data) -> {available, val} end,
%%   {T1, V1} = best_of_five(fun() -> fuse_misc:pmap(fun(_) -> Fun(state_data) end,
%%                                                   lists:seq(1, N))
%%                           end),

%%   {T2, V2} = best_of_five(fun() -> fuse_misc:pmap(
%%                                      fun(_) -> fuse:call(F, Fun) end,
%%                                      lists:seq(1, N))
%%                           end),
%%   ?assertEqual(V1, V2),
%%   ?assert(T2 < T1 * 4),
%%   ?assert(T2 < 1000000),
%%   fuse:stop(F).

%%%_* Helpers ==========================================================
%% best_of_five(F) ->
%%   R = [timer:tc(F), timer:tc(F), timer:tc(F), timer:tc(F), timer:tc(F)],
%%   hd(lists:sort(fun({T1, _}, {T2, _}) -> T1 < T2 end, R)).
