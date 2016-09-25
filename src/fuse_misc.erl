%% Copyright (c) 2014-2016 Ulf Leopold.
%%
-module(fuse_misc).

-include_lib("eunit/include/eunit.hrl").

-export([make_counter/0, pmap/2]).

%%%_* API ==============================================================
make_counter() ->
  Pid = spawn(fun() -> put(count, 1), counter_loop() end),
  fun(inc) ->
      Ref = make_ref(),
      Pid ! {self(), Ref, inc},
      receive
        {Ref, Cnt} -> Cnt
      end;
     (get) ->
      Ref = make_ref(),
      Pid ! {self(), Ref, get},
      receive
        {Ref, Cnt} -> Cnt
      end
  end.

pmap(F, L) ->
  Pid = self(),
  [receive {P, Result} -> Result end || P <- [spawn(fun() ->
                                                        Pid ! {self(), F(X)}
                                                    end) || X <- L]].

%%%_* Helpers ==========================================================
counter_loop() ->
  receive
    {Pid, Ref, inc} -> Cnt = get(count), put(count, Cnt + 1), Pid ! {Ref, Cnt};
    {Pid, Ref, get} -> Cnt = get(count), Pid ! {Ref, Cnt}
  end,
  counter_loop().

%%%_* Eunit ============================================================
counter_test() ->
  Cnt = make_counter(),
  ?assertEqual(1, Cnt(inc)),
  ?assertEqual(2, Cnt(get)),
  ?assertEqual(2, Cnt(inc)),
  ?assertEqual(3, Cnt(get)),
  ?assertEqual(3, Cnt(inc)),
  ?assertEqual(4, Cnt(get)),
  ?assertEqual(4, Cnt(inc)),
  ?assertEqual(5, Cnt(get)).

pmap_test() ->
  ?assertEqual([1, 4, 9, 16, 25], pmap(fun(X) -> X * X end, [1, 2, 3, 4, 5])).
