# justified-containers
Standard containers, with keys that carry evidence of their own presence.

# Description

Have you ever *known* that a key could be found in a certain map? Were you tempted to
reach for `fromJust` or `error` to handle the "impossible" case, when you knew that
`lookup` should give `Just v`? (and did shifting requirements ever make the impossible
become possible after all?)

`Data.Map.Justified` provides a wrapper around `Data.Map`s `Data.Map.Map` that enables you
to separate the *proof that a key is present* from the *operations using the key*. Once
you prove that a key is present, you can use it `Maybe`-free in any number of other
operations -- sometimes even operations on other maps!

None of the functions in this module can cause a run-time error, and very few
of the operations return a `Maybe` value.

See the `Data.Map.Justified.Tutorial` module for usage examples.

```haskell
    withMap test_table $ \\table -> do
    
      case member 1 table of
    
        Nothing  -> putStrLn "Sorry, I couldnt prove that the key is present."
    
        Just key -> do
          -- In this do-block, \key\ represents the key 1, but carries type-level
          -- evidence that the key is present. Lookups and updates can now proceed
          -- without the possibility of error.
          putStrLn ("Found key: " ++ show key)
    
          -- lookup returns a value directly, not a \Maybe\!
          putStrLn ("Value for key: " ++ lookup key table)
    
          -- If you update an already-mapped value, the set of valid keys does
          -- not change. So the evidence that 'key' could be found in 'table'
          -- is still sufficient to ensure that 'key' can be found in the updated
          -- table as well.
          let table = reinsert key "howdy" table
          putStrLn ("Value for key in updated map: " ++ lookup key table)
```

Output:

    Found key: Key 1
    Value for key: hello
    Value for key in updated map: howdy

## Motivation: `Data.Map` and `Maybe` values

Suppose you have a key-value mapping using `Data.Map`s type `Data.Map.Map k v`. Anybody making
use of `Data.Map.Map k v` to look up or modify a value must take into account the possibility
that the given key is not present.

In `Data.Map`, there are two strategies for dealing with absent keys:

  1. Cause a runtime error (e.g. `Data.Map`s `Data.Map.!` when the key is absent)

  2. Return a `Maybe` value (e.g. `Data.Map`s `Data.Map.lookup`)

The first option introduces partial functions, so is not very palatable. But what is
wrong with the second option?

To understand the problem with returning a `Maybe` value, lets ask what the  `Maybe v` in

```haskell
    lookup :: k -> Map k v -> Maybe v
```

really does for us. By returning
a `Maybe v` value, `lookup key table` is saying "Your program must account
for the possibility that `key` cannot be found in `table`. I will ensure that you
account for this possibility by forcing you to handle the `Nothing` case."
In effect, `Data.Map` is requiring the user to prove they have handled the
possibility that a key is absent whenever they use the `Data.Map.lookup` function.

## Laziness (the bad kind)

Every programmer has probably had the experience of knowing, somehow, that a certain
key is going to be present in a map. In this case, the `Maybe v` feels like a burden:
I already *know* that this key is in the map, why should I have to handle the `Nothing` case?

In this situation, it is tempting to reach for the partial function `Data.Maybe.fromJust`,
or a pattern match like `Nothing -> error "The impossible happened!"`. But as parts of
the program are changed over time, you may find the impossible has become possible after
all (or perhaps youll see the dreaded and unhelpful `*** Exception: Maybe.fromJust: Nothing`)

It is tempting to reach for partial functions or "impossible" runtime errors here, because
the programmer has proven that the key is a member of the map in some other way. They
know that `Data.Map.lookup` should return a `Just v` --- but the *compiler* doesnt know this!

The idea behind `Data.Map.Justified` is to encode the programmers knowledge that a key
is present *within the type system*, where it can be checked at compile-time. Once a key
is known to be present, `Data.Map.Justified.lookup` will never fail. Your justification
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
`Data.Map.Justified`s `Data.Map.Justified.member` function. In `Data.Map`, `Data.Map.member`
has the type

```haskell
    member :: Ord k => k -> Map k v -> Bool
```

and reports whether or not the key can be found in the map. In `Data.Map.Justified`,
`Data.Map.Member` has the type

```haskell
    member :: Ord k => k -> Map ph k v -> Maybe (Key ph k)
```

Instead of a boolean, `Data.Map.Justified.member` either says `the key is not present`
(`Nothing`), or gives back the same key, *augmented with evidence that they key*
*is present*. This key-plus-evidence can then be used to do any number of `Maybe`-free
operations on the map.

`Data.Map.Justified` uses the same rank-2 polymorphism trick used in the `Control.Monad.ST` monad to
ensure that the `ph` phantom type can not be extracted; in effect, the proof that a key is
present can't leak to contexts where the proof would no longer be valid.
