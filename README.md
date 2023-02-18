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
> incr
> count
```

This should how the following in the first terminal:

```
Output:    L Unit
New state: Int 1

Output:    R (Int 1)
New state: Int 1
```

Where `L Unit` is the output from `incr` and `R (Int 1)` the output from `count`.

Next we will upgrade the state machine from the REPL:

```
> import Example.Counter
> upgrade (Upgrade counterSM counterSM2 upgradeState)
> incr
> incr
> count
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

If we try to upgrade again, we get an error:

```
The version running isn't the one the upgrade expects. Aborting upgrade.
```


## How it works

The basic idea is that we want our state machines to be seralisable so that we
can send them over the network in order to perform remote upgrades.

The key observation is that a state machine of type:

```
  SM state input output = input -> state -> (state, output)
```

is an instance of `Arrow` and `Arrow`s allow us to express functions an a
first-order way, as long as `arr :: Arrow a => (b -> c) -> a b c` is *not* used.

The `Arrow` type class modulo `arr` is the `CartesianCategory` type class from
Conal Elliott's work on [compiling to
categories](http://conal.net/papers/compiling-to-categories/).

The `CartesianCategory` type class is defined as follows:

```haskell
class Category k => Cartesian k where
  (&&&) :: k a c -> k a d -> k a (c, d)
  (***) :: k b c -> k b' c' -> k (b, b') (c, c')
  fst   :: k (a, b) a
  snd   :: k (a, b) b
```

The initial (or free) `CartesianCategory` is given by the following data type:

```hasell
data FreeCC c where
  Id      :: FreeCC a a
  Compose :: FreeCC b c -> FreeCC a b -> FreeCC a c
  (:&&&)  :: FreeCC a c -> FreeCC a d -> FreeCC a (c, d)
  (:***)  :: FreeCC b c -> FreeCC b' c' -> FreeCC (b, b') (c, c')
  Fst     :: FreeCC (a, b) a
  Snd     :: FreeCC (a, b) b
```

with the, hopefully, obvious `Cartesian` instance.

So the idea is that we write our program using the `Cartesian`:

```haskell
swap :: Cartesian k => k (a, b) (b, a)
swap = copy >>> snd *** fst
  where
    copy :: Cartesian k => k a (a, a)
    copy = id &&& id
```

And then we can instantiate `k` to be `FreeCC` and get ahold of the serialisable
syntax.

Ideally, since writing larger programs in this point-free style is tricky, we'd
like to the arrow syntax:

```haskell
swap' :: Cartesian k => k (a, b) (b, a)
swap' = proc (x, y) -> returnA -< (y, x)
```

After all `Cartesian` is merely `Arrow` without `arr` and we've shown how `swap`
can be implemented without `arr`, but alas GHC nevertheless tries to translate
`swap'` into something that uses `arr` which ruins our plan.

Conal developed the `concat` GHC plugin to avoid this problem. It translates any
monomorphic Haskell function into an `Arrow` of any user-defined Haskell
Cartesian closed category (CCC)[^1].

Oleg Grenrus also developed another GHC
[plugin](https://github.com/phadej/overloaded/blob/master/src/Overloaded/Categories.hs)
that does the right thing and translates arrow syntax into `CartesianCategory`
rather than `Arrow` which also solves the problem.

Since both of these approaches rely on the GHC plugin machinery they are quite
heavyweight.

Conal's translation works for any monomorphic function, so in a sense it solves
a more general problem than we need.

Oleg's library is also solving a bunch of other problems that we don't care
about, it implements OverloadedStrings, OverloadedLists, OverloadedLabels using
the plugin, and more importantly it doesn't compile with GHC 9.2 or above.

More recently Lucas Escot
[showed](https://acatalepsie.fr/posts/overloading-lambda) how to use ideas from
Jean-Philippe Bernardy and Arnaud Spiwack's
[paper](https://arxiv.org/abs/2103.06195) *Evaluating Linear Functions to
Symmetric Monoidal Categories* (2021) to provide a small DSL which gives us
something close to the arrow syntax.

It's also not quite perfect, in particular higher-order combinators cannot be
expressed, but Lucas tells me that he's working on a follow up post which
tackles this problem.

Anyway, we use the trick that Lucas described to express our state machines and
from that we something similar to the free Cartesian category (`FreeCC` above),
which we then compile to the [Categorical abstract
machine](https://en.wikipedia.org/wiki/Categorical_abstract_machine) (CAM).

This compilation process is rather straight-forward as CAM is similar to `FreeCC`.

The CAM "bytecode" is our serialised state machine and this is what gets sent
over the network when doing upgrades.

The idea is that each deployed node runs a CAM, when we deploy the node we
specify a initial state machine to run there and then we can remotely upgrade
the state machine on the node by sending it CAM bytecode of the old SM (this is
used to verify that we are not updating the wrong SM), the bytecode for the new
SM and the bytecode for a state migration (old state to new state). the state
migration is type-safe.

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
  + https://github.com/ghc-proposals/ghc-proposals/pull/303 ?
- [ ] Use application and releases for
      [upgrades](https://kennyballou.com/blog/2016/12/elixir-hot-swapping/index.html),
      also see how this can be automated using rebar3 over
      [here](https://lrascao.github.io/automatic-release-upgrades-in-erlang/).

- [ ] Can we implement the abstract machine and event loop using
      [Cosmopolitan](https://github.com/jart/cosmopolitan) for portability?

## See also

* [`essence-of-live-coding`](https://github.com/turion/essence-of-live-coding):
  FRP library with hot code swapping support.
* Dan Piponi's `circuit`s are similar to our state machines:
   - http://blog.sigfpe.com/2017/01/addressing-pieces-of-state-with.html
   -  http://blog.sigfpe.com/2017/01/building-free-arrows-from-components.html

## Acknowledgment

Thanks to Daniel Gustafsson for helping me understand `Port` from the
*Overloading the lambda abstraction in Haskell* post!


[^1]: The closed part of Cartesian *closed* category means that we also add
    exponents (not just finite products), i.e. analogous to `ArrowApply`.
