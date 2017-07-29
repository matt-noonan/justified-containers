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
-- A wrapper around 'Data.Map' for shifting the burden of proof that a key
-- exists in a map from "lookup time" to "key creation time".
--

module Data.Map.Justified (
    -- * Map and Key types
      Map
    , Key
    , theKey
    , theMap

    -- * Evaluation
    , withMap

    -- * Gathering evidence
    , member
    , keys

    -- * Lookup
    , lookup
    , (!)
      
    ) where

import Prelude hiding (lookup)
import qualified Data.Map as M

{--------------------------------------------------------------------
  Map and Key types
--------------------------------------------------------------------}
-- | A 'Data.Map' value that allows direct lookup of keys that
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

-- | Get the underlying 'Data.Map' out of a 'Map'.
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

{--------------------------------------------------------------------
  Gathering evidence
--------------------------------------------------------------------}
-- | /O(log n)/. Obtain evidence that the key is a member of the map.
--
-- Where 'Data.Map' generally requires evidence that a key exists in a map
-- at every use of some functions (e.g. 'Data.Map.lookup'),
-- 'Map' requires the evidence up-front. After it is known that a key can be
-- found, there is no need for 'Maybe' types or run-time errors.
--
-- The @Maybe value@ that has to be checked at every lookup in 'Data.Map'
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
  Lookup
--------------------------------------------------------------------}
-- | /O(log n)/. Find the value at a key. Unlike
-- 'Data.Map.(!)', this function is total and can not fail at runtime.
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

