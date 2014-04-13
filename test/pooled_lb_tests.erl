%% Copyright (c) 2014 Ulf Leopold.
-module(pooled_lb_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ae(X, Y), ?assertEqual(X, Y)).

%% Four workers in pool (w1, w2, w3, w4). Load balancer over two boxes
%% (b1, b2).
pool_in_front_of_lb_test() ->
  Probe = fun(X) -> {available, X} end,
  Tmos = [1000],
  Log = fun(F, A) -> ?debugFmt(F, A) end,
  {ok, P} = fuse_pool:start_link([{w1, Tmos, Probe}, {w2, Tmos, Probe},
                                  {w3, Tmos, Probe}, {w4, Tmos, Probe}], Log),
  {ok, Lb} = fuse_lb:start_link([{b1, Tmos, Probe}, {b2, Tmos, Probe}],
                                round_robin, Log),

  Work = fun(D) -> timer:sleep(1000), {available, D} end,
  LbCall = fun(_) ->
               {available, fuse_lb:call(Lb, Work)}
           end,

  s(fun() -> ?ae({ok, b1}, fuse_pool:call(P, LbCall)) end),
  s(fun() -> ?ae({ok, b2}, fuse_pool:call(P, LbCall)) end),
  s(fun() -> ?ae({ok, b1}, fuse_pool:call(P, LbCall)) end),
  s(fun() -> ?ae({ok, b2}, fuse_pool:call(P, LbCall)) end),
  s(fun() -> ?ae({ok, b1}, fuse_pool:call(P, LbCall)) end),
  s(fun() -> ?ae({ok, b2}, fuse_pool:call(P, LbCall)) end),
  timer:sleep(100),
  ?ae(0, fuse_pool:available_fuses(P)),
  ?ae(2, fuse_pool:queued_work(P)),
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
