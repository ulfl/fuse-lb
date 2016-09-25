# Fuse-LB

Simple Erlang application for load balancing requests to a pool of
resources. Handling retries in case of failures.

## Building

### Dependencies
Requires rebar in your path.

#### Building Fuse-LB

    $ make

## Running unit tests

    $ make eunit


## Example

    > Probe = fun(X) -> {available, X} end.
    #Fun<erl_eval.6.82930912>
    > {ok, Lb} = fuse_lb:start_link([{user_data1, [3000], Probe},
                                     {user_data2, [3000], Probe},
                                     {user_data3, [3000], Probe}],
                                     round_robin).
    {ok,<0.322.0>}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data1}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data2}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data3}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data1}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data2}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data3}
    > fuse_lb:call(Lb, fun(Data) -> {unavailable, Data} end).  %% Make fuse 1 unavailable.
    {ok,user_data1}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data2}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data3}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data2}
    >
    > %% Wait 3s.
    >
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data3}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data1}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data2}
    > fuse_lb:call(Lb, fun(Data) -> {available, Data} end).
    {ok,user_data3}


# TODO

* Upgrade to R18 and move away from erlang:now().
* Switch to rebar3.

Fuse-LB: Copyright (c) 2014-2016 Ulf Leopold.
