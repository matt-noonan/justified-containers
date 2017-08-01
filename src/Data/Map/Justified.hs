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
-- Have you ever /known/ that a key could be found in a certain map? Were you tempted to
-- reach for @'fromJust'@ or @'error'@ to handle the "impossible" case, when you knew that
-- @'lookup'@ should give @'Just' v@? (and did shifting requirements ever make the impossible
-- become possible after all?)
--
-- "Data.Map.Justified" provides a wrapper around "Data.Map"'s @'Data.Map.Map'@ that enables you
-- to separate the /proof that a key is present/ from the /operations using the key/. Once
-- you prove that a key is present, you can use it @Maybe@-free in any number of other
-- operations -- sometimes even operations on other maps!
--
-- None of the functions in this module can cause a run-time error, and very few
-- of the operations return a @'Maybe'@ value.
--
-- See the 'Data.Map.Justified.Tutorial' module for usage examples.
--
-- @
--  withMap test_table $ \\table -> do
--  
--    case member 1 table of
--
--      Nothing  -> putStrLn "Sorry, I couldn't prove that the key is present."
--
--      Just key -> do
--        -- In this do-block, \'key\' represents the key 1, but carries type-level
--        -- evidence that the key is present. Lookups and updates can now proceed
--        -- without the possibility of error.
--        putStrLn ("Found key: " ++ show key)
--  
--        -- lookup returns a value directly, not a \'Maybe\'!
--        putStrLn ("Value for key: " ++ lookup key table)
--  
--        -- If you update an already-mapped value, the set of valid keys does
--        -- not change. So the evidence that \'key\' could be found in \'table\'
--        -- is still sufficient to ensure that \'key\' can be found in the updated
--        -- table as well.
--        let table' = reinsert key "howdy" table
--        putStrLn ("Value for key in updated map: " ++ lookup key table')
-- @
-- Output:
--
-- @
--  Found key: Key 1
--  Value for key: hello
--  Value for key in updated map: howdy
-- @
--
-- == Motivation: "Data.Map" and @'Maybe'@ values
--
-- Suppose you have a key-value mapping using "Data.Map"'s type @'Data.Map.Map' k v@. Anybody making
-- use of @'Data.Map.Map' k v@ to look up or modify a value must take into account the possibility
-- that the given key is not present.
--
-- In "Data.Map", there are two strategies for dealing with absent keys:
--
--   1. Cause a runtime error (e.g. "Data.Map"'s @'Data.Map.!'@ when the key is absent)
--
--   2. Return a @'Maybe'@ value (e.g. "Data.Map"'s @'Data.Map.lookup'@)
--
-- The first option introduces partial functions, so is not very palatable. But what is
-- wrong with the second option?
--
-- To understand the problem with returning a @'Maybe'@ value, let's ask what returning
-- @Maybe v@ from @'Data.Map.lookup' :: k -> Map k v -> Maybe v@ really does for us. By returning
-- a @Maybe v@ value, @lookup key table@ is saying "Your program must account
-- for the possibility that @key@ cannot be found in @table@. I will ensure that you
-- account for this possibility by forcing you to handle the @'Nothing'@ case."
-- In effect, "Data.Map" is requiring the user to prove they have handled the
-- possibility that a key is absent whenever they use the @'Data.Map.lookup'@ function.
--
-- == Laziness (the bad kind)
--
-- Every programmer has probably had the experience of knowing, somehow, that a certain
-- key is going to be present in a map. In this case, the @'Maybe' v@ feels like a burden:
-- I already /know/ that this key is in the map, why should I have to handle the @'Nothing'@ case?
--
-- In this situation, it is tempting to reach for the partial function @'Data.Maybe.fromJust'@,
-- or a pattern match like @'Nothing' -> 'error' "The impossible happened!"@. But as parts of
-- the program are changed over time, you may find the impossible has become possible after
-- all (or perhaps you'll see the dreaded and unhelpful @*** Exception: Maybe.fromJust: Nothing@)
--
-- It is tempting to reach for partial functions or "impossible" runtime errors here, because
-- the programmer has proven that the key is a member of the map in some other way. They
-- know that @'Data.Map.lookup'@ should return a @'Just' v@ --- but the /compiler/ doesn't know this!
--
-- The idea behind "Data.Map.Justified" is to encode the programmer's knowledge that a key
-- is present /within the type system/, where it can be checked at compile-time. Once a key
-- is known to be present, @'Data.Map.Justified.lookup'@ will never fail. Your justification
-- removes the @'Just'@!
--
-- == How it works
--
-- Evidence that a key can indeed be found in a map is carried by a phantom type parameter @ph@
-- shared by both the @'Data.Map.Justified.Map'@ and @'Data.Map.Justified.Key'@ types. If you are
-- able to get your hands on a value of type @'Key' ph k@, then you must have already proven that
-- the key is present in /any/ value of type @'Map' ph k v@.
--
-- The @'Key' ph k@ type is simply a @newtype@ wrapper around @k@, but the phantom type @ph@ allows
-- @'Key' ph k@ to represent both /a key of type @k@/ __and__ /a proof that the key is present in/
-- /all maps of type @'Map' ph k v@/.
--
-- There are several ways to prove that a key belongs to a map, but the simplest is to just use
-- "Data.Map.Justified"'s @'Data.Map.Justified.member'@ function. In "Data.Map", @'Data.Map.member'@
-- has the type
--
-- @'Data.Map.member' :: 'Ord' k => k -> 'Data.Map.Map' k v -> 'Bool'@
--
-- and reports whether or not the key can be found in the map. In "Data.Map.Justified",
-- @'Data.Map.Member'@ has the type
--
-- @'member' :: 'Ord' k => k -> 'Map' ph k v -> 'Maybe' ('Key' ph k)@
--
-- Instead of a boolean, @'Data.Map.Justified.member'@ either says "the key is not present"
-- (@'Nothing'@), or gives back the same key, /augmented with evidence that they key/
-- /is present/. This key-plus-evidence can then be used to do any number of @'Maybe'@-free
-- operations on the map.
--
-- "Data.Map.Justified" uses the same rank-2 polymorphism trick used in the @'Control.Monad.ST'@ monad to
-- ensure that the @ph@ phantom type can not be extracted; in effect, the proof that a key is
-- present can't leak to contexts where the proof would no longer be valid.
--

module Data.Map.Justified (
    -- * Map and Key types
      Map
    , Key
    , theKey
    , theMap

    -- * Evaluation
    , withMap
    , withSingleton
    , KeyInfo(..)
    , MissingReference
    , withRecMap

    -- * Gathering evidence
    , member
    , keys
    , lookupLT
    , lookupLE
    , lookupGT
    , lookupGE
      
    -- * Lookup and update
    , lookup
    , (!)
    , adjust
    , adjustWithKey
    , reinsert
    -- ** Inserting new keys
    , inserting
    , insertingWith
      
    -- * Mapping
    , mapWithKey
    , traverseWithKey
    , mapAccum
    , mapAccumWithKey
    -- ** Mapping keys
    , mappingKeys
    , mappingKnownKeys
    , mappingKeysWith
    , mappingKnownKeysWith
      
    -- * Unions
    , unioning
    , unioningWith
    , unioningWithKey

    -- * Zipping
    , zip
    , zipWith
    , zipWithKey
      
    -- * Indexing
    , findIndex
    , elemAt

    -- * Utilities
    , tie

    ) where

import Prelude hiding (lookup, zip, zipWith)
import qualified Data.Map as M
import Data.List (partition)
import Control.Arrow ((&&&))

{--------------------------------------------------------------------
  Map and Key types
--------------------------------------------------------------------}
-- | A "Data.Map" @'Data.Map.Map'@ wrapper that allows direct lookup of keys that
-- are known to exist in the map.
--
-- Here, "direct lookup" means that once a key has been proven
-- to exist in the map, it can be used to extract a value directly
-- from the map, rather than requiring a @'Maybe'@ layer.
--
-- @'Map'@ allows you to shift the burden of proof that a key exists
-- in a map from "prove at every lookup" to "prove once per key".
newtype Map ph k v = Map (M.Map k v) deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A key that knows it can be found in certain @'Map'@s.
-- 
-- The evidence that the key can be found in a map is carried by
-- the type system via the phantom type parameter @ph@. Certain
-- operations such as lookup will only type-check if the @'Key'@
-- and the @'Map'@ have the same phantom type parameter.
newtype Key ph k = Key k deriving (Eq, Ord, Show)

-- | Get a bare key out of a key-plus-evidence by forgetting
-- what map the key can be found in.
theKey :: Key ph k -> k
theKey (Key k) = k

-- | Get the underlying "Data.Map" @'Data.Map'@ out of a @'Map'@.
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
withMap m cont = cont (Map m)

-- | Like @'withMap'@, but begin with a singleton map taking @k@ to @v@.
--
-- The continuation is passed a pair consisting of:
--
--  1. Evidence that @k@ is in the map, and
--
--  2. The singleton map itself, of type @'Map' ph k v@.
--
-- > withSingleton 1 'a' (uncurry lookup) == 'a'

withSingleton :: k -> v -> (forall ph. (Key ph k, Map ph k v) -> t) -> t
withSingleton k v cont = cont (Key k, Map (M.singleton k v))

-- | Information about whether a key is present or missing.
-- See @'Data.Map.Justified.withRecMap'@ and "Data.Map.Justified.Tutorial"'s @'Data.Map.Justified.Tutorial.example5'@.
data KeyInfo = Present | Missing deriving (Show, Eq, Ord)

-- | A description of what key/value-containing-keys pairs failed to be found.
-- See @'Data.Map.Justified.withRecMap'@ and "Data.Map.Justified.Tutorial"'s @'Data.Map.Justified.Tutorial.example5'@.
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
--
-- See @'Data.Map.Justified.Tutorial.example5'@ for more usage examples.
withRecMap :: (Ord k, Traversable f)
           => M.Map k (f k)
           -> (forall ph. Map ph k (f (Key ph k)) -> t)
           -> Either [MissingReference k f] t
withRecMap m cont =
  case bad of
    [] -> Right $ cont (Map $ M.map (fmap Key) $ M.fromList ok)
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
-- at every use of some functions (e.g. "Data.Map"'s @'Data.Map.lookup'@),
-- @'Map'@ requires the evidence up-front. After it is known that a key can be
-- found, there is no need for @'Maybe'@ types or run-time errors.
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
-- "Data.Map"'s @'Data.Map.!'@, this function is total and can not fail at runtime.
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

-- | /O(log n)/. Find the largest key smaller than the given one
-- and return the corresponding (key,value) pair, with evidence for the key.
--
-- > withMap (M.fromList [(3,'a'), (5,'b')]) $ \table -> lookupLT 3 table == Nothing
-- > withMap (M.fromList [(3,'a'), (5,'b')]) $ \table -> lookupLT 4 table == Just (Key 3, 'a')
lookupLT :: Ord k => k -> Map ph k v -> Maybe (Key ph k, v)
lookupLT k (Map m) = fmap (\(key, v) -> (Key key, v)) (M.lookupLT k m)

-- | /O(log n)/. Find the smallest key greater than the given one
-- and return the corresponding (key,value) pair, with evidence for the key.
--
-- > withMap (M.fromList [(3,'a'), (5,'b')]) $ \table -> lookupGT 4 table == Just (Key 5, 'b')
-- > withMap (M.fromList [(3,'a'), (5,'b')]) $ \table -> lookupGT 5 table == Nothing
lookupGT :: Ord k => k -> Map ph k v -> Maybe (Key ph k, v)
lookupGT k (Map m) = fmap (\(key, v) -> (Key key, v)) (M.lookupGT k m)

-- | /O(log n)/. Find the largest key smaller than or equal to the given one
-- and return the corresponding (key,value) pair, with evidence for the key.
--
-- > withMap (M.fromList [(3,'a'), (5,'b')]) $ \table -> lookupLE 2 table == Nothing
-- > withMap (M.fromList [(3,'a'), (5,'b')]) $ \table -> lookupLE 4 table == Just (Key 3, 'a')
-- > withMap (M.fromList [(3,'a'), (5,'b')]) $ \table -> lookupLE 5 table == Just (Key 5, 'b')
lookupLE :: Ord k => k -> Map ph k v -> Maybe (Key ph k, v)
lookupLE k (Map m) = fmap (\(key, v) -> (Key key, v)) (M.lookupLE k m)

-- | /O(log n)/. Find the smallest key greater than or equal to the given one
-- and return the corresponding (key,value) pair, with evidence for the key.
--
-- > withMap (M.fromList [(3,'a'), (5,'b')]) $ \table -> lookupGE 3 table == Just (Key 3, 'a')
-- > withMap (M.fromList [(3,'a'), (5,'b')]) $ \table -> lookupGE 4 table == Just (Key 5, 'b')
-- > withMap (M.fromList [(3,'a'), (5,'b')]) $ \table -> lookupGE 6 table == Nothing
lookupGE :: Ord k => k -> Map ph k v -> Maybe (Key ph k, v)
lookupGE k (Map m) = fmap (\(key, v) -> (Key key, v)) (M.lookupGE k m)

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

adjustWithKey :: Ord k => (Key ph k -> v -> v) -> Key ph k -> Map ph k v -> Map ph k v
adjustWithKey f (Key k) (Map m) = Map (M.adjustWithKey f' k m)
  where f' key = f (Key key)

-- | Replace the value at a key, known to be in the map.
--
-- Since the set of valid keys in the input map and output map
-- are the same, keys that were valid for the input map remain
-- valid for the output map.

reinsert :: Ord k => Key ph k -> v -> Map ph k v -> Map ph k v
reinsert (Key k) v (Map m) = Map (M.insert k v m)

-- | Insert a value for a key that is /not/ known to be in the map,
-- evaluating the updated map with the given continuation.
--
-- The continuation is given three things:
--
--   1. A proof that the inserted key exists in the new map,
--
--   2. A function that can be used to convert evidence that a key
--      exists in the original map, to evidence that the key exists in
--      the updated map, and
--
--   3. The updated @'Data.Map.Justified.Map'@, with a /different phantom type/.
--
-- > withMap (M.fromList [(5,'a'), (3,'b')]) (\table -> inserting 5 'x' table $ \(_,_,table') -> theMap table') == M.fromList [(3, 'b'), (5, 'x')]
-- > withMap (M.fromList [(5,'a'), (3,'b')]) (\table -> inserting 7 'x' table $ \(_,_,table') -> theMap table') == M.fromList [(3, 'b'), (5, 'b'), (7, 'x')]
--
-- See @'Data.Map.Justified.Tutorial.example4'@ for more usage examples.
inserting :: Ord k
          => k
          -> v
          -> Map ph k v
          -> (forall ph'. (Key ph' k, Key ph k -> Key ph' k, Map ph' k v) -> t)
          -> t
inserting k v (Map m) cont = cont (Key k, \(Key key) -> Key key, Map (M.insert k v m))

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertingWith' f key value mp cont@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key /does/ exist, the function will
-- insert the pair @(key, f new_value old_value)@.
--
-- The continuation is given three things (as in @'inserting'@):
--
--   1. A proof that the inserted key exists in the new map,
--
--   2. A function that can be used to convert evidence that a key
--      exists in the original map, to evidence that the key exists in
--      the updated map, and
--
--   3. The updated @'Data.Map.Justified.Map'@, with a /different phantom type/.
--
-- > withMap (M.fromList [(5,"a"), (3,"b")]) (theMap . insertingWith (++) 5) == M.fromList [(3,"b"), (5,"xxxa")]
-- > withMap (M.fromList [(5,"a"), (3,"b")]) (theMap . insertingWith (++) 7) == M.fromList [(3,"b"), (5,"a"), (7,"xxx")]
insertingWith :: Ord k
              => (v -> v -> v)
              -> k
              -> v
              -> Map ph k v
              -> (forall ph'. (Key ph' k, Key ph k -> Key ph' k, Map ph' k v) -> t)
              -> t
insertingWith f k v (Map m) cont = cont (Key k, \(Key key) -> Key key, Map (M.insertWith f k v m))
                                         
{--------------------------------------------------------------------
  Unions
--------------------------------------------------------------------}

-- | Take the left-biased union of two @'Data.Map.Justified.Map'@s, as in "Data.Map"'s
-- @'Data.Map.union'@, evaluating the unioned map with the given continuation.
--
-- The continuation is given three things:
--
--   1. A function that can be used to convert evidence that a key exists in the left
--      map to evidence that the key exists in the union,
--
--   2. A function that can be used to convert evidence that a key exists in the right
--      map to evidence that the key exists in the union, and
--
--   3. The updated @'Data.Map.Justified.Map'@, with a /different phantom type/.
--
unioning :: Ord k
         => Map phL k v
         -> Map phR k v
         -> (forall ph'. (Key phL k -> Key ph' k, Key phR k -> Key ph' k, Map ph' k v) -> t)
         -> t
unioning (Map mapL) (Map mapR) cont = cont (\(Key key) -> Key key,
                                            \(Key key) -> Key key,
                                            Map (M.union mapL mapR))

-- | @'unioningWith' f@ is the same as @'unioning'@, except that @f@ is used to
-- combine values that correspond to keys found in both maps.
unioningWith :: Ord k
             => (v -> v -> v)
             -> Map phL k v
             -> Map phR k v
             -> (forall ph'. (Key phL k -> Key ph' k, Key phR k -> Key ph' k, Map ph' k v) -> t)
             -> t
unioningWith f (Map mapL) (Map mapR) cont = cont (\(Key key) -> Key key,
                                                  \(Key key) -> Key key,
                                                  Map (M.unionWith f mapL mapR))

-- | @'unioningWithKey' f@ is the same as @'unioningWith' f@, except that @f@ also
-- has access to the key and evidence that it is present in both maps.
unioningWithKey :: Ord k
                => (Key phL k -> Key phR k -> v -> v -> v)
                -> Map phL k v
                -> Map phR k v
                -> (forall ph'. (Key phL k -> Key ph' k, Key phR k -> Key ph' k, Map ph' k v) -> t)
                -> t
unioningWithKey f (Map mapL) (Map mapR) cont = cont (\(Key key) -> Key key,
                                                     \(Key key) -> Key key,
                                                     Map (M.unionWithKey f' mapL mapR))
  where f' k = f (Key k) (Key k)

{--------------------------------------------------------------------
  Mapping and traversing
--------------------------------------------------------------------}

-- | /O(n)/. Map a function over all keys and values in the map.
--
mapWithKey :: (Key ph k -> a -> b)
           -> Map ph k a
           -> Map ph k b
mapWithKey f (Map m) = Map (M.mapWithKey f' m)
  where f' k = f (Key k)

-- | /O(n)/. As in @'Data.Map.traverse'@: traverse the map, but give the
-- traversing function access to the key associated with each value.
traverseWithKey :: Applicative t
                => (Key ph k -> a -> t b)
                -> Map ph k a
                -> t (Map ph k b)
traverseWithKey f (Map m) = fmap Map (M.traverseWithKey f' m)
  where f' k = f (Key k)
        
-- | /O(n)/. The function @'mapAccum'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
mapAccum :: (a -> b -> (a,c))
         -> a
         -> Map ph k b
         -> (a, Map ph k c)
mapAccum f a (Map m) = fmap Map (M.mapAccum f a m)

-- | /O(n)/. The function @'mapAccumWithKey'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
mapAccumWithKey :: (a -> Key ph k -> b -> (a,c))
                -> a
                -> Map ph k b
                -> (a, Map ph k c)
mapAccumWithKey f a (Map m) = fmap Map (M.mapAccumWithKey f' a m)
  where f' x k = f x (Key k)
        
-- | /O(n*log n)/.
-- @'mappingKeys'@ evaluates a continuation with the map obtained by applying
-- @f@ to each key of @s@.
--
-- The size of the resulting map may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- The continuation is passed two things:
--
--   1. A function that converts evidence that a key belongs to the original map
--      into evidence that a key belongs to the new map.
--
--   2. The new, possibly-smaller map.
--
--
mappingKeys :: Ord k2
            => (k1 -> k2)
            -> Map ph k1 v
            -> (forall ph'. (Key ph k1 -> Key ph' k2, Map ph' k2 v) -> t)
            -> t
mappingKeys f (Map m) cont = cont (\(Key k) -> Key (f k), Map (M.mapKeys f m))

-- | /O(n*log n)/.
-- Same as @'mappingKeys'@, but the key-mapping function can make use of
-- evidence that the input key belongs to the original map.
--
mappingKnownKeys :: Ord k2
            => (Key ph k1 -> k2)
            -> Map ph k1 v
            -> (forall ph'. (Key ph k1 -> Key ph' k2, Map ph' k2 v) -> t)
            -> t
mappingKnownKeys f (Map m) cont = cont (Key . f, Map (M.mapKeys f' m))
  where f' k = f (Key k)
        
-- | /O(n*log n)/.
-- Same as @'mappingKeys'@, except a function is used to combine values when
-- two or more keys from the original map correspond to the same key in the
-- final map.
mappingKeysWith :: Ord k2
                => (v -> v -> v)
                -> (k1 -> k2)
                -> Map ph k1 v
                -> (forall ph'. (Key ph k1 -> Key ph' k2, Map ph' k2 v) -> t)
                -> t
mappingKeysWith op f (Map m) cont = cont (\(Key k) -> Key (f k), Map (M.mapKeysWith op f m))

-- | /O(n*log n)/.
-- Same as @'mappingKnownKeys'@, except a function is used to combine values when
-- two or more keys from the original map correspond to the same key in the
-- final map.
mappingKnownKeysWith :: Ord k2
                => (v -> v -> v)
                -> (Key ph k1 -> k2)
                -> Map ph k1 v
                -> (forall ph'. (Key ph k1 -> Key ph' k2, Map ph' k2 v) -> t)
                -> t
mappingKnownKeysWith op f (Map m) cont = cont (Key . f, Map (M.mapKeysWith op f' m))
  where f' k = f (Key k)
        
{--------------------------------------------------------------------
  Zipping
--------------------------------------------------------------------}

-- | Zip the values in two maps together. The phantom type @ph@ ensures
-- that the two maps have the same set of keys, so no elements are left out.
--
zip :: Ord k
    => Map ph k a
    -> Map ph k b
    -> Map ph k (a,b)
zip = zipWith (,)

-- | Combine the values in two maps together. The phantom type @ph@ ensures
-- that the two maps have the same set of keys, so no elements are left out.
zipWith :: Ord k
        => (a -> b -> c)
        -> Map ph k a
        -> Map ph k b
        -> Map ph k c
zipWith f m1 m2 = mapWithKey (\k x -> f x (m2 ! k)) m1

-- | Combine the values in two maps together, using the key and values.
-- The phantom type @ph@ ensures that the two maps have the same set of
-- keys.
zipWithKey :: Ord k
           => (Key ph k -> a -> b -> c)
           -> Map ph k a
           -> Map ph k b
           -> Map ph k c
zipWithKey f m1 m2 = mapWithKey (\k x -> f k x (m2 ! k)) m1
        
{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}
-- | /O(log n)/. Return the /index/ of a key, which is its zero-based index in
-- the sequence sorted by keys. The index is a number from /0/ up to, but not
-- including, the size of the map. The index also carries a proof that it is
-- valid for the map.
--
-- Unlike "Data.Map"'s @'Data.Map.findIndex'@, this function can not fail at runtime.

findIndex :: Ord k => Key ph k -> Map ph k a -> Key ph Int
findIndex (Key k) (Map m) = Key (M.findIndex k m)

-- | /O(log n)/. Retrieve an element by its /index/, i.e. by its zero-based
-- index in the sequence sorted by keys.
--
-- Unlike "Data.Map"'s @'Data.Map.elemAt'@, this function can not fail at runtime.

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
