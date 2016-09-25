%% Copyright (c) 2014-2016 Ulf Leopold.
-module(fuse_lb_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ae(X, Y), ?assertEqual(X, Y)).

round_robin_test() ->
  Probe = fun(X) -> {unavailable, X} end,
  Tmos = [1000],
  {ok, Lb} = fuse_lb:start_link([{1, Tmos, Probe}, {2, Tmos, Probe},
                                 {3, Tmos, Probe}], round_robin),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)).

round_robin_failure_test() ->
  Probe = fun(X) -> {available, X} end,
  Tmos = [1000],
  {ok, Lb} = fuse_lb:start_link([{1, Tmos, Probe}, {2, Tmos, Probe},
                                 {3, Tmos, Probe}], round_robin),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {unavailable, UserData} end)),
  ?ae(2, fuse_lb:num_fuses_active(Lb)),
  timer:sleep(10),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {unavailable, UserData} end)),
  ?ae(1, fuse_lb:num_fuses_active(Lb)),
  timer:sleep(10),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {unavailable, UserData} end)),
  ?ae(0, fuse_lb:num_fuses_active(Lb)),
  ?ae({error, no_fuses_left}, fuse_lb:call(Lb, fun(UserData) ->
                                                   {available, UserData}
                                               end)),
  ?ae({error, no_fuses_left}, fuse_lb:call(Lb, fun(UserData) ->
                                                   {available, UserData}
                                               end)),
  ?ae({error, no_fuses_left}, fuse_lb:call(Lb, fun(UserData) ->
                                                   {available, UserData}
                                               end)),
  timer:sleep(2000),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)).

round_robin_probe_test() ->
  Cnt = fuse_misc:make_counter(),
  Probe = fun(X) ->
              case Cnt(inc) >= 3 of
                true  -> {available, X};
                false -> {unavailable, X}
              end
          end,
  Tmos = [100],
  {ok, Lb} = fuse_lb:start_link([{1, Tmos, Probe}, {2, Tmos, Probe},
                                 {3, Tmos, Probe}], round_robin),
  ?ae(1, Cnt(get)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {unavailable, UserData} end)),

  timer:sleep(105),
  ?ae(2, Cnt(get)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),

  timer:sleep(105),
  ?ae(3, Cnt(get)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),

  timer:sleep(105),
  ?ae(4, Cnt(get)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),

  timer:sleep(105),
  ?ae(4, Cnt(get)).

round_robin_backoff_test() ->
  Cnt = fuse_misc:make_counter(),
  Probe = fun(X) ->
              case Cnt(inc) >= 5 of
                true  -> {available, X};
                false -> {unavailable, X}
              end
          end,
  Tmos = [{10, 2}, {50, 1}, {500, 1}, 1000],
  {ok, Lb} = fuse_lb:start_link([{1, Tmos, Probe}, {2, Tmos, Probe},
                                 {3, Tmos, Probe}], round_robin),
  ?ae(1, Cnt(get)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {unavailable, UserData} end)),

  timer:sleep(22),
  ?ae(3, Cnt(get)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),

  timer:sleep(52),
  ?ae(4, Cnt(get)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),

  timer:sleep(502),
  ?ae(5, Cnt(get)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),

  timer:sleep(500),
  ?ae(5, Cnt(get)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),

  timer:sleep(502),
  ?ae(6, Cnt(get)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),

  timer:sleep(1000),
  ?ae(6, Cnt(get)).

prio_alg_test() ->
  Probe = fun(X) -> {unavailable, X} end,
  Tmos = [1000],
  {ok, Lb} = fuse_lb:start_link([{1, Tmos, Probe}, {2, Tmos, Probe},
                                 {3, Tmos, Probe}], prio),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {unavailable, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {unavailable, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {unavailable, UserData} end)),
  ?ae({error, no_fuses_left}, fuse_lb:call(Lb, fun(UserData) ->
                                                   {available, UserData}
                                               end)),
  ?ae({error, no_fuses_left}, fuse_lb:call(Lb, fun(UserData) ->
                                                   {available, UserData}
                                               end)).

prio_alg_failure_test() ->
  Probe = fun(X) -> {available, X} end,
  {ok, Lb} = fuse_lb:start_link([{1, [2000], Probe}, {2, [1000], Probe},
                                 {3, [500], Probe}],
                                prio, fun(S, A) -> ?debugFmt(S, A) end),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {unavailable, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_lb:call(Lb, fun(UserData) -> {unavailable, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_lb:call(Lb, fun(UserData) -> {unavailable, UserData} end)),
  ?ae({error, no_fuses_left}, fuse_lb:call(Lb, fun(UserData) ->
                                                   {available, UserData}
                                               end)),
  timer:sleep(3005),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_lb:call(Lb, fun(UserData) -> {available, UserData} end)).
