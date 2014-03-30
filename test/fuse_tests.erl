%% Copyright (c) 2014 Ulf Leopold.
-module(fuse_tests).

-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ============================================================
call_not_burnt_test() ->
  {ok, F} = fuse:start_link(my_data, [500], fun(my_data) ->
                                                {available, my_data}
                                            end, self()),
  ?assertEqual({available, value}, fuse:call(F, fun(my_data) ->
                                                    {available, value}
                                                end)),
  fuse:stop(F).

call_burnt_test() ->
  {ok, F} = fuse:start_link(my_data, [500], fun(my_data) ->
                                                {available, my_data}
                                            end, self()),
  ?assertEqual({unavailable, someval}, fuse:call(F, fun(my_data) ->
                                                        {unavailable, someval}
                                                    end)),
  ?assertEqual({error, fuse_burnt}, fuse:call(F, fun(my_data) ->
                                                     error(should_not_be_called)
                                                 end)),
  timer:sleep(600),
  ?assertEqual({available, value}, fuse:call(F, fun(my_data) ->
                                                    {available, value}
                                                end)),
  fuse:stop(F).

call_burnt_multiple_test() ->
  Cnt = fuse_misc:make_counter(),
  {ok, F} = fuse:start_link(my_data, [{1, 10}, {10, 10}, 50],
                            fun(my_data) ->
                                case Cnt(inc) of
                                  21 -> {available, my_data};
                                  _  -> {unavailable, my_data}
                                end
                            end, self()),
  ?assertEqual({unavailable, someval}, fuse:call(F, fun(my_data) ->
                                                        {unavailable, someval}
                                                    end)),
  ?assertEqual({error, fuse_burnt}, fuse:call(F, fun(my_data) ->
                                                     error(should_not_be_called)
                                                 end)),
  timer:sleep(200),
  ?assertEqual(22, Cnt(get)),
  ?assertEqual({available, value}, fuse:call(F, fun(my_data) ->
                                                    {available, value}
                                                end)),
  receive
    {_, {re_fuse, F}} -> ok
  end,
  fuse:stop(F).

update_state_test() ->
  {ok, F} = fuse:start_link(start_state, [50], fun(start_state) ->
                                                   {available, new_state}
                                               end, self()),
  ?assertEqual({unavailable, someval}, fuse:call(F, fun(start_state) ->
                                                        {unavailable, someval}
                                                    end)),
  ?assertEqual({error, fuse_burnt}, fuse:call(F, fun(start_state) ->
                                                     error(should_not_be_called)
                                                 end)),
  timer:sleep(100),
  ?assertEqual({available, someval}, fuse:call(F, fun(new_state) ->
                                                      {available, someval}
                                                  end)),
  receive
    {_, {re_fuse, F}} -> ok
  end,
  fuse:stop(F).

%% Prove that fuse can do at least 10,000 dispatches in a second and
%% that it is less than 4 times as expensive as calling a fun.
perf_test() ->
  N = 10000,
  {ok, F} = fuse:start_link(my_data, [{1, 10}, {10, 10}, 50],
                            fun(my_data) -> {available, my_data} end, self()),
  Fun = fun(my_data) -> {available, val} end,
  {T1, V1} = best_of_five(fun() -> fuse_misc:pmap(fun(_) -> Fun(my_data) end,
                                                  lists:seq(1, N))
                          end),

  {T2, V2} = best_of_five(fun() -> fuse_misc:pmap(
                                     fun(_) -> fuse:call(F, Fun) end,
                                     lists:seq(1, N))
                          end),
  ?assertEqual(V1, V2),
  ?assert(T2 < T1 * 4),
  ?assert(T2 < 1000000),
  fuse:stop(F).

%%%_* Helpers ==========================================================
best_of_five(F) ->
  R = [timer:tc(F), timer:tc(F), timer:tc(F), timer:tc(F), timer:tc(F)],
  hd(lists:sort(fun({T1, _}, {T2, _}) -> T1 < T2 end, R)).
