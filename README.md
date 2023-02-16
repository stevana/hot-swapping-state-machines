# hot-swapping-state-machines

An experiment in implementing remote hot code swapping, or dynamic code upgrade,
for state machines.

## Background

In Erlang it's possible to seamlessly hot swap the code on a running process.

Consider the following `gen_server` implementation of a counter which can be
`incr`emented and the current `count` value retrieved:

```erlang
-module(counter).
-version("1").

-export([start_link/0, incr/0, count/0]).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{debug, [trace]}]).

incr()  -> gen_server:call(?MODULE, incr).
count() -> gen_server:call(?MODULE, count).

init([]) -> {ok, 0}.

handle_call(incr, _From, State) -> {reply, ok, State+1};
handle_call(count, _From, State) -> {reply, State, State};
handle_call(_Call, _From, State) -> {noreply, State}.

handle_cast(_Cast, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
```

Here's a small REPL session which shows how it works:

```
1> c(counter).
{ok,counter}
2> counter:start_link().
{ok,<0.87.0>}
3> counter:incr().
*DBG* counter got call incr from <0.80.0>
*DBG* counter sent ok to <0.80.0>, new state 1
ok
4> counter:incr().
*DBG* counter got call incr from <0.80.0>
*DBG* counter sent ok to <0.80.0>, new state 2
ok
5> counter:count().
*DBG* counter got call count from <0.80.0>
*DBG* counter sent 2 to <0.80.0>, new state 2
2
```

Now lets introduce a contrived change to the counter where we change the state
to contain two counters, the two operations `incr` and `count` will operate on
the second counter and the first counter will be the old counter that was there
before we introduced this change. The state migration, in `code_change`, will
use the value of the old counter for the new counter.

```diff
@@ -1,5 +1,5 @@
 -module(counter).
--version("1").
+-version("2").

 -export([start_link/0, incr/0, count/0]).

@@ -13,14 +13,13 @@

 init([]) -> {ok, 0}.

-handle_call(incr, _From, State) -> {reply, ok, State+1};
-handle_call(count, _From, State) -> {reply, State, State};
+handle_call(incr, _From, {OldState, State}) -> {reply, ok, {OldState, State+1}};
+handle_call(count, _From, {OldState, State}) -> {reply, State, {OldState, State}};
 handle_call(_Call, _From, State) -> {noreply, State}.

 handle_cast(_Cast, State) -> {noreply, State}.
 handle_info(_Info, State) -> {noreply, State}.

 terminate(_Reason, _State) -> ok.

-code_change(_OldVsn, State, _Extra) -> {ok, State}.
+code_change("1", State, _Extra) -> {ok, {State, State}}.
```

We can now upgrade the running process as follows:

```
6> compile:file(counter).
{ok,counter}
7> sys:suspend(counter).
ok
8> code:purge(counter).
false
9> code:load_file(counter).
{module,counter}
10> sys:change_code(counter, counter, "1", []).
ok
11> sys:resume(counter).
ok
12> counter:incr().
*DBG* counter got call incr from <0.80.0>
*DBG* counter sent ok to <0.80.0>, new state {2,3}
ok
13> counter:incr().
*DBG* counter got call incr from <0.80.0>
*DBG* counter sent ok to <0.80.0>, new state {2,4}
ok
14> counter:count().
*DBG* counter got call count from <0.80.0>
*DBG* counter sent 4 to <0.80.0>, new state {2,4}
4
```

This repo is an experiment which tries to do something similar in Haskell for
state machines of type `input -> state -> (state, output)`.

## Usage

In one terminal run `cabal run exe` and in another terminal run `cabal repl` and
type:

```
> import LibMain
> tick
> cget
```

This should how the following in the first terminal:

```
Output:    L Unit
New state: Int 1

Output:    R (Int 1)
New state: Int 1
```

Where `L Unit` is the output from `tick` and `R (Int 1)` the output from `cget`.

Next we will upgrade the state machine from the REPL:

```
> import Example.Counter
> upgrade (Upgrade counterSM counterSM2 upgradeState)
> tick
> tick
> cget
```

Which will result in the following being printed in the first terminal:

```
Upgrade successful!

Output:    L Unit
New state: Pair (Int 1) (Int 1)

Output:    L Unit
New state: Pair (Int 1) (Int 2)

Output:    R (Int 2)
New state: Pair (Int 1) (Int 2)
```

## How it works

XXX:

## Contributing

- [ ] Generate `FreeFunc s a b` so that the correctness can be tested;
- [ ] Backwards compatibility, i.e. allow old inputs after an upgrade;
- [ ] Rollback?
- [ ] Better syntax
  + [Overloading the lambda abstraction in
    Haskell](https://acatalepsie.fr/posts/overloading-lambda);
  + [arrowp-qq](https://hackage.haskell.org/package/arrowp-qq): A preprocessor and
    quasiquoter for translating arrow notation;
  + [Overloaded.Categories](https://hackage.haskell.org/package/overloaded-0.3.1/docs/Overloaded-Categories.html)
    plugin;
- [ ] Use application and releases for
      [upgrades](https://kennyballou.com/blog/2016/12/elixir-hot-swapping/index.html),
      also see how this can be automated using rebar3 over
      [here](https://lrascao.github.io/automatic-release-upgrades-in-erlang/).

## See also

* [`essence-of-live-coding`](https://github.com/turion/essence-of-live-coding):
  FRP library with hot code swapping support.

## Acknowledgment

Thanks to Daniel Gustafsson for helping me understand `Port` from the
*Overloading the lambda abstraction in Haskell* post!
