# hot-swapping-state-machines

An experiment in implementing remote hot code swapping, or dynamic code upgrade,
for state machines.

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
