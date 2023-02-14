# hot-swapping-state-machines

An experiment in implementing remote hot code swapping for state machines.

## Background

In Erlang it's possilbe to seamlessly hot code swap a running process.

```erlang
XXX: example
```

## Usage

In one terminal run `cabal run exe` and in another terminal run `cabal repl` and type:

```
> import LibMain
> tick
> cget
```

This should how the following in the first terminal:

```
L Unit
R (Int 1)
```

Where `L Unit` is the output from `tick` and `R (Int 1)` the output from `cget`.

Next we will upgrade the state machine from the REPL:

```
> import Compiler
> import Example.Counter
> load (compile counterSM2)
> tick
> cget
```

Which will result in the following being printed in the first terminal:

```
Upgraded!
L Unit
R (Int 3)
```

## How it works


## See also

* [arrowp-qq](https://hackage.haskell.org/package/arrowp-qq): A preprocessor and
  quasiquoter for translating arrow notation;

* [Overloaded.Categories](https://hackage.haskell.org/package/overloaded-0.3.1/docs/Overloaded-Categories.html)
  plugin;

* [Overloading the lambda abstraction in
  Haskell](https://acatalepsie.fr/posts/overloading-lambda);

* [`essence-of-live-coding`](https://github.com/turion/essence-of-live-coding):
  FRP library with hot code swapping support.

## Acknowledgment

Thanks to Daniel Gustafsson for helping me understand `Port` from the
*Overloading the lambda abstraction in Haskell* post!
