%% Copyright (c) 2014-2016 Ulf Leopold.
-module(fuse_pool_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ae(X, Y), ?assertEqual(X, Y)).

give_time_to_initialize_fuses() -> timer:sleep(7).

probe_available(FuseNum) ->
  timer:sleep(random:uniform(4)),
  {available, FuseNum}.

probe_unavailable({init, FuseNum}) ->
  timer:sleep(random:uniform(4)),
  {available, FuseNum};
probe_unavailable(FuseNum) ->
  timer:sleep(random:uniform(4)),
  {unavailable, FuseNum}.

simple_calls_test() ->
  Probe = fun probe_unavailable/1,
  Tmos = [1000],
  {ok, P} = fuse_pool:start_link([{{init, 1}, Tmos, Probe},
                                  {{init, 2}, Tmos, Probe},
                                  {{init, 3}, Tmos, Probe}], 5000),
  give_time_to_initialize_fuses(),
  ?ae({ok, 1}, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)).

unavailable_test() ->
  Probe = fun probe_unavailable/1,
  Tmos = [1000],
  {ok, P} = fuse_pool:start_link([{{init, 1}, Tmos, Probe},
                                  {{init, 2}, Tmos, Probe},
                                  {{init, 3}, Tmos, Probe}], 5000),
  give_time_to_initialize_fuses(),
  ?ae({ok, 1}, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_pool:call(P, fun(UserData) -> {unavailable, UserData} end)),
  ?ae(2, fuse_pool:num_fuses_active(P)),
  ?ae({ok, 2}, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_pool:call(P, fun(UserData) -> {unavailable, UserData} end)),
  ?ae(1, fuse_pool:num_fuses_active(P)),
  ?ae({ok, 3}, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 3}, fuse_pool:call(P, fun(UserData) -> {unavailable, UserData} end)),
  ?ae(0, fuse_pool:num_workers_idle(P)).

recovery1_test() ->
  Probe = fun probe_available/1,
  Tmos = [1000],
  {ok, P} = fuse_pool:start_link([{1, Tmos, Probe}, {2, Tmos, Probe},
                                  {3, Tmos, Probe}], 5000),
  give_time_to_initialize_fuses(),
  ?ae({ok, 1}, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_pool:call(P, fun(UserData) -> {unavailable, UserData} end)),
  ?ae({ok, 2}, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 2}, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae(2, fuse_pool:num_workers_idle(P)),
  timer:sleep(1500),
  ?ae(3, fuse_pool:num_workers_idle(P)),
  ?ae({ok, 1}, fuse_pool:call(P, fun(UserData) -> {available, UserData} end)),
  ?ae({ok, 1}, fuse_pool:call(P, fun(UserData) -> {unavailable, UserData} end)).

queue_test() ->
  Probe = fun probe_available/1,
  Tmos = [1000],
  Work = fun({D, T}) -> timer:sleep(T), {available, D} end,
  {ok, P} = fuse_pool:start_link([{{1, 500}, Tmos, Probe},
                                  {{2, 600}, Tmos, Probe},
                                  {{3, 700}, Tmos, Probe}], 5000),
  give_time_to_initialize_fuses(),
  s(fun() -> ?ae({ok, 1}, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae({ok, 2}, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae({ok, 3}, fuse_pool:call(P, Work)) end),
  timer:sleep(100),
  ?ae(0, fuse_pool:num_workers_idle(P)),
  s(fun() -> ?ae({ok, 1}, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae({ok, 2}, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae({ok, 3}, fuse_pool:call(P, Work)) end),
  timer:sleep(100),
  ?ae(3, fuse_pool:num_jobs_queued(P)),
  w(6),
  ?ae(0, fuse_pool:num_jobs_queued(P)),
  ?ae(3, fuse_pool:num_workers_idle(P)).

recovery2_test() ->
  Probe = fun probe_available/1,
  Tmos = [2000],
  Work = fun({D, T}) -> timer:sleep(T), {available, D} end,
  Fail = fun({D, _}) -> {unavailable, D} end,
  {ok, P} = fuse_pool:start_link([{{1, 500}, Tmos, Probe},
                                  {{2, 600}, Tmos, Probe},
                                  {{3, 700}, Tmos, Probe}],
                                 5000, fun(F, A) -> ?debugFmt(F, A) end),
  give_time_to_initialize_fuses(),
  s(fun() -> ?ae({ok, 1}, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae({ok, 2}, fuse_pool:call(P, Fail)) end),
  s(fun() -> ?ae({ok, 3}, fuse_pool:call(P, Work)) end),
  timer:sleep(100),
  ?ae(0, fuse_pool:num_workers_idle(P)),
  s(fun() -> ?ae({ok, 1}, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae({ok, 3}, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae({ok, 1}, fuse_pool:call(P, Work)) end),
  timer:sleep(100),
  ?ae(3, fuse_pool:num_jobs_queued(P)),
  w(6),
  ?ae(0, fuse_pool:num_jobs_queued(P)),
  ?ae(2, fuse_pool:num_workers_idle(P)),
  timer:sleep(1500),
  ?ae(3, fuse_pool:num_workers_idle(P)),
  s(fun() -> ?ae({ok, 1}, fuse_pool:call(P, Work)) end),
  w(1).

queue_tmo_test() ->
  Probe = fun probe_available/1,
  Tmos = [2000],
  Work = fun(D) -> timer:sleep(3000), {available, D} end,
  {ok, P} = fuse_pool:start_link([{1, Tmos, Probe},
                                  {2, Tmos, Probe}],
                                 1000, fun(F, A) -> ?debugFmt(F, A) end),
  give_time_to_initialize_fuses(),
  s(fun() -> ?ae({ok, 1}, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae({ok, 2}, fuse_pool:call(P, Work)) end),
  s(fun() -> ?ae({error, fuse_pool_queue_tmo}, fuse_pool:call(P, Work)) end),
  w(3).

%%%_* Helpers ==========================================================
s(Fun) ->
  Pid = spawn_link(Fun),
  erlang:monitor(process, Pid).

w(0) -> ok;
w(N) ->
  receive
    {'DOWN', _, _, _, _} -> w(N - 1)
  end.
