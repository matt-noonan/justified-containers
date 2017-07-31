{-# LANGUAGE RankNTypes, DeriveTraversable #-}
-- |
-- Module      :  Data.Map.Justified
-- Copyright   :  (c) Matt Noonan 2017
-- License     :  BSD-style
-- Maintainer  :  matt.noonan@gmail.com
-- Portability :  portable
--
-- = Description
--
-- A wrapper around "Data.Map"'s 'Data.Map.Map' for shifting the burden of proof that a key
-- exists in a map from "lookup time" to "key creation time".
--
-- == Motivation: "Data.Map" and 'Maybe' values
--
-- Suppose you have a key-value mapping using "Data.Map"'s type @Map k v@. Anybody making
-- use of @Map k v@ to look up or modify a value must take into account the possibility
-- that the given key is not present.
--
-- In "Data.Map", there are two strategies for dealing with absent keys:
--
--   1. Cause a runtime error (e.g. "Data.Map"'s 'Data.Map.!' when the key is absent)
--
--   2. Return a 'Maybe' value (e.g. "Data.Map"'s 'Data.Map.lookup')
--
-- The first option introduces partial functions, so is not very palatable. But what is
-- wrong with the second option?
--
-- To understand the problem with returning a 'Maybe' value, let's ask what returning
-- @Maybe v@ from @lookup :: k -> Map k v -> Maybe v@ really does for us. By returning
-- a @Maybe v@ value, @lookup key table@ is saying "Your program must account
-- for the possibility that @key@ cannot be found in @table@. I will ensure that you
-- account for this possibility by forcing you to handle the 'Nothing' case."
-- In effect, "Data.Map" is requiring the user to prove they have handled the
-- possibility that a key is absent whenever they use the 'Data.Map.lookup' function.
--
-- == Laziness (the bad kind)
--
-- Every programmer has probably had the experience of knowing, somehow, that a certain
-- key is going to be present in a map. In this case, the @Maybe v@ feels like a burden:
-- I already /know/ that this key is in the map, why should I handle the 'Nothing' case?
--
-- In this situation, it is tempting to reach for the partial function 'Data.Maybe.fromJust',
-- or a pattern match like @Nothing -> error "The impossible happened!"@. But as parts of
-- the program are changed over time, you may find the impossible has become possible after
-- all (or perhaps you'll see the dreaded and unhelpful @*** Exception: Maybe.fromJust: Nothing@)
--
-- It is tempting to reach for partial functions or "impossible" runtime errors here, because
-- the programmer has proven that the key is a member of the map in some other way. They
-- know that 'Data.Map.lookup' should return a 'Just v' --- but the /compiler/ doesn't know this!
--
-- The idea behind "Data.Map.Justified" is to encode the programmer's knowledge that a key
-- is present, within the type system where it can be checked at compile-time. Once a key
-- is known to be present, 'Data.Map.Justified.lookup' will never fail. Your justification
-- removes the 'Just'!
--
-- == How it works
--
-- Evidence that a key can indeed be found in a map is carried by a phantom type parameter @ph@
-- shared by both the 'Data.Map.Justified.Map' and 'Data.Map.Justified.Key' types. If you are
-- able to get your hands on a value of type @Key ph k@, then you must have already proven that
-- the key is present in /any/ value of type @Map ph k v@.
--
-- The @Key ph k@ type is simply a @newtype@ wrapper around @k@, but the phantom type @ph@ allows
-- @Key ph k@ to represent both /a key of type @k@/ __and__ /a proof that the key is present in
-- maps of type @Map ph k v@/.
--
-- There are several ways to prove that a key belongs to a map, but the simplest is to just use
-- "Data.Map.Justified"'s 'Data.Map.Justified.member' function. In "Data.Map", 'Data.Map.member'
-- has the type
--
-- @member :: Ord k => k -> Map k v -> Bool@
--
-- and reports whether or not the key can be found in the map. In "Data.Map.Justified",
-- 'Data.Map.Member' has the type
--
-- @member :: Ord k => k -> Map ph k v -> Maybe (Key ph k)@
--
-- Instead of a boolean, 'Data.Map.Justified.member' either says "the key is not present"
-- ('Nothing'), or gives back the same key, /augmented with evidence that they key/
-- /is present/. This key-plus-evidence can then be used to do any number of 'Maybe'-free
-- operations on the map.
--
-- "Data.Map.Justified" uses the same rank-2 polymorphism trick used in the 'ST' monad to
-- ensure that the @ph@ phantom type can not be extracted; in effect, the proof that a key is
-- present can't leak to contexts where the proof would no longer be valid.
--
-- == Tutorial
--
-- See "Data.Map.Justified.Tutorial" for usage examples and FAQs.

module Data.Map.Justified (
    -- * Map and Key types
      Map
    , Key
    , theKey
    , theMap

    -- * Evaluation
    , withMap
    , KeyInfo(..)
    , MissingReference
    , withRecMap

    -- * Gathering evidence
    , member
    , keys

    -- * Lookup and update
    , lookup
    , (!)
    , adjust
    , adjustWithKey
    , reinsert

    -- * Indexing
    , findIndex
    , elemAt

    -- * Utilities
    , tie

    ) where

import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.List (partition)
import Control.Arrow ((&&&))

{--------------------------------------------------------------------
  Map and Key types
--------------------------------------------------------------------}
-- | A "Data.Map" 'Data.Map.Map' wrapper that allows direct lookup of keys that
-- are known to exist in the map.
--
-- Here, "direct lookup" means that once a key has been proven
-- to exist in the map, it can be used to extract a value directly
-- from the map, rather than requiring a 'Maybe' layer.
--
-- 'Map' allows you to shift the burden of proof that a key exists
-- in a map from "prove at every lookup" to "prove once per key".
newtype Map ph k v = Map (M.Map k v) deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A key that knows it can be found in certain 'Map's.
-- 
-- The evidence that the key can be found in a map is carried by
-- the type system via the phantom type parameter @ph@. Certain
-- operations such as lookup will only type-check if the 'Key'
-- and the 'Map' have the same phantom type parameter.
newtype Key ph k = Key k deriving (Eq, Ord, Show)

-- | Get a bare key out of a key-plus-evidence by forgetting
-- what map the key can be found in.
theKey :: Key ph k -> k
theKey (Key k) = k

-- | Get the underlying "Data.Map" 'Data.Map' out of a 'Map'.
theMap :: Map ph k v -> M.Map k v
theMap (Map m) = m

{--------------------------------------------------------------------
  Evaluation
--------------------------------------------------------------------}
-- | Evaluate an expression using justified key lookups into the given map.
--
-- > import qualified Data.Map as M
-- >
-- > withMap (M.fromList [(1,"A"), (2,"B")]) $ \m -> do
-- >
-- >   -- prints "Found Key 1 with value A"
-- >   case member 1 m of
-- >     Nothing -> putStrLn "Missing key 1."
-- >     Just k  -> putStrLn ("Found " ++ show k ++ " with value " ++ lookup k m)
-- >
-- >   -- prints "Missing key 3."
-- >   case member 3 m of
-- >     Nothing -> putStrLn "Missing key 3."
-- >     Just k  -> putStrLn ("Found " ++ show k ++ " with value " ++ lookup k m)

withMap :: M.Map k v -> (forall ph. Map ph k v -> t) -> t
withMap m f = f (Map m)

-- | Information about whether a key is present or missing.
-- See 'Data.Map.Justified.withRecMap' and "Data.Map.Justified.Tutorial"'s
-- 'Data.Map.Justified.Tutorial.example4'.
data KeyInfo = Present | Missing deriving (Show, Eq, Ord)

-- | A description of what key/value-containing-keys pairs failed to be found.
-- See 'Data.Map.Justified.withRecMap' and "Data.Map.Justified.Tutorial"'s
-- 'Data.Map.Justified.Tutorial.example4'.
type MissingReference k f = (k, f (k, KeyInfo))

-- | Evaluate an expression using justified key lookups into the given map,
-- when the values can contain references back to keys in the map.
--
-- Each referenced key is checked to ensure that it can be found in the map.
-- If all referenced keys are found, they are augmented with evidence and the
-- given function is applied.
-- If some referenced keys are missing, information about the missing references
-- is generated instead.
--
-- > import qualified Data.Map as M
-- >
-- > data Cell ptr = Nil | Cons ptr ptr deriving (Functor, Foldable, Traversable)
-- >
-- > memory1 = M.fromList [(1, Cons 2 1), (2, Nil)]
-- > withRecMap memory1 (const ()) -- Right ()
-- >
-- > memory2 = M.fromList [(1, Cons 2 3), (2, Nil)]
-- > withRecMap memory2 (const ()) -- Left [(1, Cons (2,Present) (3,Missing))]

withRecMap :: (Ord k, Traversable f)
           => M.Map k (f k)
           -> (forall ph. Map ph k (f (Key ph k)) -> t)
           -> Either [MissingReference k f] t
withRecMap m f =
  case bad of
    [] -> Right $ f (Map $ M.map (fmap Key) $ M.fromList ok)
    _  -> Left (map (\(k,v) -> (k, fmap (id &&& locate) v)) bad)
  where
    (ok, bad) = partition (all ((== Present) . locate) . snd) (M.toList m)
    locate k = if M.member k m then Present else Missing

{--------------------------------------------------------------------
  Gathering evidence
--------------------------------------------------------------------}
-- | /O(log n)/. Obtain evidence that the key is a member of the map.
--
-- Where "Data.Map" generally requires evidence that a key exists in a map
-- at every use of some functions (e.g. "Data.Map"'s 'Data.Map.lookup'),
-- 'Map' requires the evidence up-front. After it is known that a key can be
-- found, there is no need for 'Maybe' types or run-time errors.
--
-- The @Maybe value@ that has to be checked at every lookup in "Data.Map"
-- is then shifted to a @Maybe (Key ph k)@ that has to be checked in order
-- to obtain evidence that a key is in the map.
--
-- Note that the "evidence" only exists at the type level, during compilation;
-- there is no runtime distinction between keys and keys-plus-evidence.
--
-- > withMap (M.fromList [(5,'a'), (3,'b')]) (isJust . member 1) == False
-- > withMap (M.fromList [(5,'a'), (3,'b')]) (isJust . member 5) == True
member :: Ord k => k -> Map ph k v -> Maybe (Key ph k)
member k (Map m) = fmap (const $ Key k) (M.lookup k m)

-- | A list of all of the keys in a map, along with proof
-- that the keys exist within the map.
keys :: Map ph k v -> [Key ph k]
keys (Map m) = map Key (M.keys m)


{--------------------------------------------------------------------
  Lookup and update
--------------------------------------------------------------------}
-- | /O(log n)/. Find the value at a key. Unlike
-- "Data.Map"'s 'Data.Map.!', this function is total and can not fail at runtime.
(!) :: Ord k => Map ph k v -> Key ph k -> v
(!) = flip lookup

-- | /O(log n)/. Lookup the value at a key, known to be in the map.
--
-- The result is a @v@ rather than a @Maybe v@, because the
-- proof obligation that the key is in the map must already
-- have been discharged to obtain a value of type @Key ph k@.
--
lookup :: Ord k => Key ph k -> Map ph k v -> v
lookup (Key k) (Map m) = case M.lookup k m of
  Just value -> value
  Nothing    -> error "Data.Map.Justified has been subverted!"

-- | Adjust the valid at a key, known to be in the map,
-- using the given function.
--
-- Since the set of valid keys in the input map and output map
-- are the same, keys that were valid for the input map remain
-- valid for the output map.

adjust :: Ord k => (v -> v) -> Key ph k -> Map ph k v -> Map ph k v
adjust f (Key k) (Map m) = Map (M.adjust f k m)

-- | Adjust the valid at a key, known to be in the map,
-- using the given function.
--
-- Since the set of valid keys in the input map and output map
-- are the same, keys that were valid for the input map remain
-- valid for the output map.

adjustWithKey :: Ord k => (k -> v -> v) -> Key ph k -> Map ph k v -> Map ph k v
adjustWithKey f (Key k) (Map m) = Map (M.adjustWithKey f k m)

-- | Replace the value at a key, known to be in the map.
--
-- Since the set of valid keys in the input map and output map
-- are the same, keys that were valid for the input map remain
-- valid for the output map.

reinsert :: Ord k => Key ph k -> v -> Map ph k v -> Map ph k v
reinsert (Key k) v (Map m) = Map (M.insert k v m)


{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}
-- | /O(log n)/. Return the /index/ of a key, which is its zero-based index in
-- the sequence sorted by keys. The index is a number from /0/ up to, but not
-- including, the 'size' of the map. The index also carries a proof that it is
-- valid for the map.
--
-- Unlike "Data.Map"'s 'Data.Map.findIndex', this function can not fail at runtime.

findIndex :: Ord k => Key ph k -> Map ph k a -> Key ph Int
findIndex (Key k) (Map m) = Key (M.findIndex k m)

-- | /O(log n)/. Retrieve an element by its /index/, i.e. by its zero-based
-- index in the sequence sorted by keys.
--
-- Unlike "Data.Map"'s 'Data.Map.elemAt', this function can not fail at runtime.

elemAt :: Key ph Int -> Map ph k v -> (Key ph k, v)
elemAt (Key n) (Map m) = let (k,v) = M.elemAt n m in (Key k, v)

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | Build a value by "tying the knot" according to the references in the map.
tie :: (Functor f, Ord k) => (f a -> a) ->  Map ph k (f (Key ph k)) -> Key ph k -> a
tie phi m = go
  where
    go = (`lookup` table)
    table = fmap (phi . fmap go) m
