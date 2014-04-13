%% Copyright (c) 2014 Ulf Leopold.
-module(fuse_pool_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ae(X, Y), ?assertEqual(X, Y)).

simple_calls_test() ->
  Probe = fun(X) -> {unavailable, X} end,
  Tmos = [1000],
  {ok, P} = fuse_pool:start_link([{1, Tmos, Probe}, {2, Tmos, Probe},
                                  {3, Tmos, Probe}]),
  ?ae(1, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae(1, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae(1, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae(1, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)).

unavailable_test() ->
  Probe = fun(X) -> {unavailable, X} end,
  Tmos = [1000],
  {ok, P} = fuse_pool:start_link([{1, Tmos, Probe}, {2, Tmos, Probe},
                                  {3, Tmos, Probe}]),
  ?ae(1, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae(1, fuse_pool:call(P, fun(UserData) -> {unavailable, UserData} end)),
  ?ae(2, fuse_pool:num_fuses_active(P)),
  ?ae(2, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae(2, fuse_pool:call(P, fun(UserData) -> {unavailable, UserData} end)),
  ?ae(1, fuse_pool:num_fuses_active(P)),
  ?ae(3, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae(3, fuse_pool:call(P, fun(UserData) -> {unavailable, UserData} end)),
  ?ae(0, fuse_pool:num_workers_idle(P)).

recovery_test() ->
  Probe = fun(X) -> {available, X} end,
  Tmos = [1000],
  {ok, P} = fuse_pool:start_link([{1, Tmos, Probe}, {2, Tmos, Probe},
                                  {3, Tmos, Probe}]),
  ?ae(1, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae(1, fuse_pool:call(P, fun(UserData) -> {unavailable, UserData} end)),
  ?ae(2, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae(2, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae(2, fuse_pool:num_workers_idle(P)),
  timer:sleep(1500),
  ?ae(3, fuse_pool:num_workers_idle(P)),
  ?ae(1, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae(1, fuse_pool:call(P, fun(UserData) -> {unavailable, UserData} end)).

queue_test() ->
  Probe = fun(X) -> {available, X} end,
  Tmos = [1000],
  Work = fun({D, T}) -> timer:sleep(T), {available, D} end,
  {ok, P} = fuse_pool:start_link([{{1, 500}, Tmos, Probe},
                                  {{2, 600}, Tmos, Probe},
                                  {{3, 700}, Tmos, Probe}]),
  s(fun() -> ?ae(1, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae(2, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae(3, fuse_pool:call(P, Work)) end),
  timer:sleep(100),
  ?ae(0, fuse_pool:num_workers_idle(P)),
  s(fun() -> ?ae(1, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae(2, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae(3, fuse_pool:call(P, Work)) end),
  timer:sleep(100),
  ?ae(3, fuse_pool:num_jobs_queued(P)),
  w(6),
  ?ae(0, fuse_pool:num_jobs_queued(P)),
  ?ae(3, fuse_pool:num_workers_idle(P)).

reovery_test() ->
  Probe = fun(X) -> {available, X} end,
  Tmos = [2000],
  Work = fun({D, T}) -> timer:sleep(T), {available, D} end,
  Fail = fun({D, _}) -> {unavailable, D} end,
  {ok, P} = fuse_pool:start_link([{{1, 500}, Tmos, Probe},
                                  {{2, 600}, Tmos, Probe},
                                  {{3, 700}, Tmos, Probe}],
                                 fun(F, A) -> ?debugFmt(F, A) end),
  s(fun() -> ?ae(1, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae(2, fuse_pool:call(P, Fail)) end),
  s(fun() -> ?ae(3, fuse_pool:call(P, Work)) end),
  timer:sleep(100),
  ?ae(0, fuse_pool:num_workers_idle(P)),
  s(fun() -> ?ae(1, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae(3, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae(1, fuse_pool:call(P, Work)) end),
  timer:sleep(100),
  ?ae(3, fuse_pool:num_jobs_queued(P)),
  w(6),
  ?ae(0, fuse_pool:num_jobs_queued(P)),
  ?ae(2, fuse_pool:num_workers_idle(P)),
  timer:sleep(1500),
  ?ae(3, fuse_pool:num_workers_idle(P)),
  s(fun() -> ?ae(2, fuse_pool:call(P, Work)) end),
  w(1).

%%%_* Helpers ==========================================================
s(Fun) ->
  Pid = spawn_link(Fun),
  erlang:monitor(process, Pid).

w(0) -> ok;
w(N) ->
  receive
    {'DOWN', _, _, _, _} -> w(N - 1)
  end.
