# Fuse-LB

Simple Erlang application for load balancing and pooling of requests to a
collection of resources. A back-off schedule can be specified for use in case of
failures.


## Example

To illustrate how the Fuse load balancer and process pool works let's first
define some helper functions:

```erlang
Pmap = fun(F, L) -> Pid = self(), [receive {P, Result} -> Result end ||
                                   P <- [spawn(fun() -> Pid ! {self(), F(X)} end) ||
                                         X <- L]] end.

D = fun(T) -> timer:sleep(T) end.
DR = fun(T) -> timer:sleep(rand:uniform(T)) end.

T = fun(F) -> {T, R} = timer:tc(F), {T/(1000*1000), R} end.

F = fun({T, R}) -> {T, lists:foldl(fun(C, D) -> orddict:update_counter(C, 1, D) end,
                                   orddict:new(), R)} end.

FT = fun(X) -> F(T(X)) end.
```

Here `pmap` is a concurrent implementation of map. `D` generates a delay up to
the specified number of milliseconds. `DR` is similar but the delay is random.
`T` measures the execution time for a function, and `F` the frequency
distribution of items in a list. `FT` just combines `T` and `F` into one. The
example below illustrates the use of these functions.

```erlang
Work = fun(_) -> D(1000) end.
FT(fun() -> lists:map(Work, lists:seq(1, 100)) end).
=> {100.2761,[{ok,100}]}

FT(fun() -> Pmap(Work, lists:seq(1, 100)) end).
=> {1.009982,[{ok,100}]}
```

The example shows that executing work that takes 1s a hundred times in sequence
takes about 100s, whereas doing it in parallel (using 100 processes) takes about
1s. We can get this kind of speedup since the work function is not performing
actual work, it is just sleeping. The example also shows that the frequency
distrubution for the return values is 100 'ok's.

Let's assume we have three resources to which we want to balance requests. Each
resource can handle multiple requests simultaneously, we just want to make sure
that the request count is spread evenly. In case one of the resources is down,
we want requests to be directed to the other two resources instead. For this
task we can use a fuse load balancer (fuse_lb) in round robin mode:

```erlang
{ok, Lb} = fuse_lb:start_link([{s1, [1000], fun(S) -> {available, S} end},
                               {s2, [1000], fun(S) -> {available, S} end},
                               {s3, [1000], fun(S) -> {available, S} end}],
                              round_robin).

Work = fun(_) -> DR(2), {ok, R} = fuse_lb:call(Lb, fun(S) -> D(1000), {available, S} end), R end.
FT(fun() -> Pmap(Work, lists:seq(1, 100)) end).

=> {1.015221,[{s1,34},{s2,33},{s3,33}]}
```

The fuse load balancer is initialized with one fuse per resourse. The first fuse
in the example has a state variable 's1' a back-off polling schedule of "once
every second" and a polling function that just reports the fuse is available. We
are not concerned with the polling here since the fuses won't burn in this
example.

The work for the `Work` function consists of sleeping for 1s, simulating the
resource actually doing some kind of work. The work function takes the fuse
state as input and returns it without modification as output. With a hundred
parallel processes all scheduling their work via the load balancer, the
execution time is about 1s. From the frequency distribution of the return values
we see that the work was evenly spread across the three fuses.

If we modify the work function so that that all work scheduled via the first
fuse takes 2s instead of 1s, then we get:

```erlang
{ok, Lb} = fuse_lb:start_link([{s1, [1000], fun(S) -> {available, S} end},
                               {s2, [1000], fun(S) -> {available, S} end},
                               {s3, [1000], fun(S) -> {available, S} end}],
                              round_robin).

Work = fun(_) -> DR(2), {ok, R} = fuse_lb:call(Lb, fun(S) ->
                                    case S of
                                      s1 -> D(2000);
                                      _  -> D(1000)
                                    end, {available, S} end), R end.

FT(fun() -> Pmap(Work, lists:seq(1, 100)) end).

=> {2.013574,[{s1,34},{s2,33},{s3,33}]}
```

The frequency distribution is still the same. This illustrates that the load
balancer lets all requests through immediately by just dividing them up evenly
among the fuses. Total execution time is 2s this time.

Fuse_lb also offers a `prio` mode which is useful if you want to limit requests to
a single resource, but keep additional ones as spare and idling backups.

Fuse pool (fuse_pool) is similar to fuse_lb, but only allows a single request
per fuse to operate in parallel. All other requests will wait in line for their
turn. This is usuful for scenarios where you want to balance requests over a
collection of resources but limit the number of simultanous jobs. For example in
order to reduce concurrency towards the resources or reuse a fixed number of
network connections.

```erlang
{ok, Pool} = fuse_pool:start_link([{s1, [1000], fun(S) -> {available, S} end},
                                   {s2, [1000], fun(S) -> {available, S} end},
                                   {s3, [1000], fun(S) -> {available, S} end}],
                                  120000).

Work = fun(_) -> DR(2), {ok, R} = fuse_pool:call(Pool, fun(S) -> D(1000), {available, S} end), R end.
FT(fun() -> Pmap(Work, lists:seq(1, 100)) end).

=> {34.171212,[{s1,33},{s2,34},{s3,33}]}
```

As expected, with three fuses the execution time is about one third of one
hundred times 1 seconds. Since each workload is the same in size, the frequency
distribution between all fuses is even.

If the execution time for workloads via one of the fuses is slower, then as seen
in this example, the frequency distribution is no longer even and the total
execution time goes up:

```erlang
{ok, Pool} = fuse_pool:start_link([{s1, [1000], fun(S) -> {available, S} end},
                                   {s2, [1000], fun(S) -> {available, S} end},
                                   {s3, [1000], fun(S) -> {available, S} end}],
                                  120000).

Work = fun(_) -> DR(2), {ok, R} = fuse_pool:call(Pool, fun(S) ->
                                    case S of
                                      s1 -> D(2000);
                                      _  -> D(1000)
                                    end, {available, S} end), R end.

FT(fun() -> Pmap(Work, lists:seq(1, 100)) end).

=> {40.158384,[{s1,20},{s2,40},{s3,40}]}
```


## Building

Build and run the unit tests as follows.

    $ make
    $ make eunit


# TODO

* Review logging.

Fuse-LB: Copyright (c) 2014-2018 Ulf Leopold.
