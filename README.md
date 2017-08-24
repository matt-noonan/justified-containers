# justified-containers
Keyed container types with type-checked proofs of key presence.

# Table of contents

  * [Description](#description)
    * [A simple example](#example)
  * [Motivation](#motivation)
  * [How it works](#how-it-works)
    * [Phantom evidence](#phantom-evidence)


# Description

Have you ever *known* that a key could be found in a certain map? Were you tempted to
reach for `fromJust` or `error` to handle the "impossible" case, when you knew that
`lookup` should give `Just v`? (and did shifting requirements ever make the impossible
become possible after all?)

`Data.Map.Justified` provides a zero-cost `newtype` wrapper around `Data.Map.Map`
that enables you to separate the *proof that a key is present* from the *operations using the key*.
Once you prove that a key is present, you can use it `Maybe`-free in any number of other
operations -- sometimes even operations on other maps!

None of the functions in this module can cause a run-time error, and very few
of the operations return a `Maybe` value.

See the `Data.Map.Justified.Tutorial` module for usage examples, or browse the API of the
most recent release [on Hackage](https://hackage.haskell.org/package/justified-containers/docs/Data-Map-Justified.html).

### Example

```haskell
    withMap test_table $ \table -> do
    
      case member 1 table of
    
        Nothing  -> putStrLn "Sorry, I couldn't prove that the key is present."
    
        Just key -> do
        
          -- We have proven that the key is present, and can now use it Maybe-free...
          putStrLn ("Found key: "     ++ show key)
          putStrLn ("Value for key: " ++ lookup key table)

          -- ...even in certain other maps!
          let table' = reinsert key "howdy" table
          putStrLn ("Value for key in updated map: " ++ lookup key table')
```

Output:

    Found key: Key 1
    Value for key: hello
    Value for key in updated map: howdy

## Motivation: `Data.Map` and `Maybe` values

Suppose you have a key-value mapping using `Data.Map`'s type `Map k v`. Anybody making
use of `Map k v` to look up or modify a value must take into account the possibility
that the given key is not present.

In `Data.Map`, there are two strategies for dealing with absent keys:

  1. Cause a runtime error (e.g. `Data.Map`'s `(!)` when the key is absent)

  2. Return a `Maybe` value (e.g. `Data.Map`'s `lookup`)

The first option introduces partial functions, so is not very palatable. But what is
wrong with the second option?

To understand the problem with returning a `Maybe` value, let's ask what the  `Maybe v` in

```haskell
    lookup :: k -> Map k v -> Maybe v
```

really does for us. By returning
a `Maybe v` value, `lookup key table` is saying "Your program must account
for the possibility that `key` cannot be found in `table`. I will ensure that you
account for this possibility by forcing you to handle the `Nothing` case."
In effect, `Data.Map` is requiring the user to prove they have handled the
possibility that a key is absent whenever they use the `lookup` function.

## Laziness (the bad kind)

Every programmer has probably had the experience of knowing, somehow, that a certain
key is going to be present in a map. In this case, the `Maybe v` feels like a burden:
I already *know* that this key is in the map, why should I have to handle the `Nothing` case?

In this situation, it is tempting to reach for the partial function `fromJust`,
or a pattern match like `Nothing -> error "The impossible happened!"`. But as parts of
the program are changed over time, you may find the impossible has become possible after
all (or perhaps youll see the dreaded and unhelpful `*** Exception: Maybe.fromJust: Nothing`)

It is tempting to reach for partial functions or "impossible" runtime errors here, because
the programmer has proven that the key is a member of the map in some other way. They
know that lookup` should return a `Just v` --- but the *compiler* doesnt know this!

The idea behind `Data.Map.Justified` is to encode the programmers knowledge that a key
is present *within the type system*, where it can be checked at compile-time. Once a key
is known to be present, `Data.Map.Justified`'s `lookup` will never fail. Your justification
removes the `Just`!

# How it works

Evidence that a key can indeed be found in a map is carried by a phantom type parameter `ph`
shared by both the `Data.Map.Justified.Map` and `Data.Map.Justified.Key` types. If you are
able to get your hands on a value of type `Key ph k`, then you must have already proven that
the key is present in *any* value of type `Map ph k v`.

The `Key ph k` type is simply a `newtype` wrapper around `k`, but the phantom type `ph` allows
`Key ph k` to represent both *a key of type `k`* __and__ *a proof that the key is present in*
*all maps of type `Map ph k v`*.

There are several ways to prove that a key belongs to a map, but the simplest is to just use
`Data.Map.Justified`'s `member` function. In `Data.Map`, `member`
has the type

```haskell
    member :: Ord k => k -> Map k v -> Bool
```

and reports whether or not the key can be found in the map. In `Data.Map.Justified`,
`member` has the type

```haskell
    member :: Ord k => k -> Map ph k v -> Maybe (Key ph k)
```

Instead of a boolean, `Data.Map.Justified`'s `member` either says `the key is not present`
(`Nothing`), or gives back the same key, *augmented with evidence that they key*
*is present*. This key-plus-evidence can then be used to do any number of `Maybe`-free
operations on the map.

`Data.Map.Justified` uses the same rank-2 polymorphism trick used in the `Control.Monad.ST` monad to
ensure that the `ph` phantom type can not be extracted; in effect, the proof that a key is
present can't leak to contexts where the proof would no longer be valid.

## Phantom evidence

You can interpret the `ph` phantom type as a concrete *set of keys*; under this interpretation,
a value of type `Key ph k` is a key of type `k`, belonging to the subset described by `ph`.
Similarly, a `Map ph k v` is a map whose keys are exactly the subset of `k` described by `ph`.
From this perspective, the maps behave as if they were total, leading to their `Maybe`-free behavior.

## Why all the continuations?

Many of the functions in `justified-containers` make use of continuations, but why? As a case-study,
consider the basic function `withMap` that promotes a standard `Data.Map.Map` to a `Data.Map.Justified.Map':

```haskell
import Data.Map.Justified
import qualified Data.Map as M
withMap :: M.Map k v -> (forall ph. Map ph k v -> t) -> t
```

The last `(forall ph. Map ph k v -> t) -> t` part is the continuation.

The idea is that we know there is *some* set of keys `ph` belonging to this particular map, but
at compile-time we may not know exactly what it is. But it does exist, after all, so we should be
able to write

```haskell
withMap :: M.Map k v -> exists ph. Map ph k v
```

Similarly, the `inserting` function could look like

```haskell
inserting :: k -> v -> Map ph k v -> exists ph'. Map ph' k v
```

which can be read as "after inserting a key/value pair, we get a (possibly) different set of keys `ph'`".
But in this case, we actually know a bit more: first, the inserted key will be found in the new map. And
second, every key in `ph` can also be found in `ph'`. We
can encode that knowledge by giving an explicit inclusion of `ph` into `ph'`, encoded as a function of
type `Key ph k -> Key ph' k`. So we could re-write `inserting` with the type

```haskell
inserting :: k -> v -> Map ph k v -> exists ph'. (Key ph' k, Key ph k -> Key ph' k, Map ph' k v)
--                                                \_______/  \___________________/  \_________/
--                                   the new key______|                |                 |
--                                                      the inclusion__|                 |
--                                                                       the new map_____|
```

Likewise, when deleting a key from a map with keys `ph`, we get a new map with keys `ph'` along
with a guarantee that `ph'` is a subset of `ph`. Compared to `inserting`, the inclusion goes the
other way: there is an inclusion of `ph'` in `ph`, encoded as a function of type `Key ph' k -> Key ph l`.
Altogether, we could give `deleting` the type

```haskell
deleting :: k -> Map ph k v -> exists ph'. (Key ph' k -> Key ph k, Map ph' k v)
--                                          \___________________/  \_________/
--                                                    |                 |
--                            the reversed inclusion__|                 |
--                                                      the new map_____|
```

A similar pattern works for other map operations like `union`, `intersection`, `difference`, and
`filter`.

## But what about the continuations?

In the last section, we argued that `deleting` should have a type like

```haskell
deleting :: k -> Map ph k v -> exists ph'. (Key ph' k -> Key ph k, Map ph' k v)
--                             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
```

But if you check the documentation, you'll see the type

```haskell
deleting :: k -> Map ph k v -> (forall ph'. (Key ph' k -> Key ph k, Map ph' k v) -> t) -> t
--                             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
```

What happened to the underlined part of the type?

The problem is that Haskell doesn't support existential types directly: the `exists` part of
the type we wrote out is just wishful thinking. Instead, we have to go about things a little
indirectly: we'll *encode* existentially-quantified types, via *rank-2 universally-quantified types*.


The idea can be understood via the Curry-Howard correspondence:
In classical logic, we have an equivalence between the propositions `∃X.P(X)`
and `∀T. ((∀X.F(X) => T) => T)`.  It turns out that this equivalence remains valid
in constructive logic, so we can transport it via the Curry-Howard correspondence to get
an isomorphism between types:

```haskell
exists ph.  Map ph k v ~ (forall ph. Map ph k v -> t) -> t
```

In other words, instead of returning the existentially-quantified type directly
we say "tell me what you wanted to do with that existentially-quantified type,
and I'll do it for you". 