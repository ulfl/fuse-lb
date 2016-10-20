%% Copyright (c) 2014-2016 Ulf Leopold.
-module(pooled_lb_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ae(X, Y), ?assertEqual(X, Y)).

give_time_to_initialize_fuses() -> timer:sleep(10).

probe_available(FuseNum) ->
  timer:sleep(random:uniform(4)),
  {available, FuseNum}.

%% Four workers in pool (w1, w2, w3, w4). Load balancer over two boxes
%% (b1, b2).
pool_in_front_of_lb_test() ->
  Probe = fun probe_available/1,
  Tmos = [1000],
  Log = fun(F, A) -> ?debugFmt(F, A) end,
  {ok, P} = fuse_pool:start_link([{w1, Tmos, Probe}, {w2, Tmos, Probe},
                                  {w3, Tmos, Probe}, {w4, Tmos, Probe}],
                                 5000, Log),
  {ok, Lb} = fuse_lb:start_link([{b1, Tmos, Probe}, {b2, Tmos, Probe}],
                                round_robin, Log),
  give_time_to_initialize_fuses(),

  Work = fun(D) -> timer:sleep(1000), {available, D} end,
  LbCall = fun(_) ->
               {ok, R} = fuse_lb:call(Lb, Work),
               {available, R}
           end,

  s(fun() -> ?ae({ok, b1}, fuse_pool:call(P, LbCall)) end),
  s(fun() -> ?ae({ok, b2}, fuse_pool:call(P, LbCall)) end),
  s(fun() -> ?ae({ok, b1}, fuse_pool:call(P, LbCall)) end),
  s(fun() -> ?ae({ok, b2}, fuse_pool:call(P, LbCall)) end),
  s(fun() -> ?ae({ok, b1}, fuse_pool:call(P, LbCall)) end),
  s(fun() -> ?ae({ok, b2}, fuse_pool:call(P, LbCall)) end),
  timer:sleep(100),
  ?ae(0, fuse_pool:num_fuses_idle(P)),
  ?ae(2, fuse_pool:num_jobs_queued(P)),
  w(6).

lb_in_front_of_pools_test() ->
  Probe = fun probe_available/1,
  Tmos = [1000],
  Log = fun(F, A) -> ?debugFmt(F, A) end,
  {ok, Lb} = fuse_lb:start_link([{b1, Tmos, Probe}, {b2, Tmos, Probe}],
                                round_robin, Log),
  {ok, P} = fuse_pool:start_link([{w1, Tmos, Probe}, {w2, Tmos, Probe}], 5000,
                                 Log),
  give_time_to_initialize_fuses(),

  Work = fun(D) -> timer:sleep(1000), {available, D} end,
  PoolCall = fun(Box) ->
                 {ok, R} = fuse_pool:call(P, Work),
                 {available, {Box, R}}
             end,

  ?ae({ok, {b1, w1}}, fuse_lb:call(Lb, PoolCall)),
  ?ae({ok, {b2, w1}}, fuse_lb:call(Lb, PoolCall)),
  ?ae({ok, {b1, w1}}, fuse_lb:call(Lb, PoolCall)),
  ?ae({ok, {b2, w1}}, fuse_lb:call(Lb, PoolCall)).

lb_in_front_of_pools_async_test() ->
  Probe = fun probe_available/1,
  Tmos = [1000],
  Log = fun(F, A) -> ?debugFmt(F, A) end,
  {ok, P1} = fuse_pool:start_link([{p1w1, Tmos, Probe}, {p1w2, Tmos, Probe}],
                                  Log),
  {ok, P2} = fuse_pool:start_link([{p2w1, Tmos, Probe}, {p2w2, Tmos, Probe}],
                                  Log),
  {ok, Lb} = fuse_lb:start_link([{{b1, P1}, Tmos, Probe},
                                 {{b2, P2}, Tmos, Probe}],
                                round_robin, Log),
  give_time_to_initialize_fuses(),

  Work = fun(D) -> timer:sleep(500), {available, D} end,
  PoolCall = fun({Box, Pool}) ->
                 {ok, R} = fuse_pool:call(Pool, Work),
                 {available, {Box, R}}
             end,

  s(fun() -> ?ae({ok, {b1, p1w1}}, fuse_lb:call(Lb, PoolCall)) end),
  timer:sleep(50),
  s(fun() -> ?ae({ok, {b2, p2w1}}, fuse_lb:call(Lb, PoolCall)) end),
  s(fun() -> ?ae({ok, {b1, p1w2}}, fuse_lb:call(Lb, PoolCall)) end),
  timer:sleep(50),
  s(fun() -> ?ae({ok, {b2, p2w2}}, fuse_lb:call(Lb, PoolCall)) end),
  timer:sleep(50),
  ?ae(0, fuse_pool:num_fuses_idle(P1)),
  ?ae(0, fuse_pool:num_fuses_idle(P2)),
  s(fun() -> ?ae({ok, {b1, p1w1}}, fuse_lb:call(Lb, PoolCall)) end),
  s(fun() -> ?ae({ok, {b2, p2w1}}, fuse_lb:call(Lb, PoolCall)) end),
  timer:sleep(50),
  ?ae(1, fuse_pool:num_jobs_queued(P1)),
  ?ae(1, fuse_pool:num_jobs_queued(P2)),
  w(6).

%%%_* Helpers ==========================================================
s(Fun) ->
  Pid = spawn_link(Fun),
  erlang:monitor(process, Pid).

w(0) -> ok;
w(N) ->
  receive
    {'DOWN', _, _, _, _} -> w(N - 1)
  end.
