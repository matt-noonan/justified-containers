{-# LANGUAGE RankNTypes, DeriveTraversable #-}
-- |
-- Module      :  Data.Map.Justified.Tutorial
-- Copyright   :  (c) Matt Noonan 2017
-- License     :  BSD-style
-- Maintainer  :  matt.noonan@gmail.com
-- Portability :  portable
--
-- = Description
--
-- The examples below demonstrate how to use the types and functions in "Data.Map.Justified".
--
-- You should be able to simply load this module in @ghci@ to play along.
-- The import list is:
--
-- @
--  import Prelude hiding (lookup)
--  
--  import Data.Map.Justified
--  
--  import qualified Data.Map as M
--  
--  import Data.Traversable (for)
--  import Data.Char (toUpper)
--  import Control.Monad (forM_)
-- @

module Data.Map.Justified.Tutorial where

import Prelude hiding (lookup)

import Data.Map.Justified

import qualified Data.Map as M

import Data.Char (toUpper)
import Control.Monad (forM_)

import Data.Type.Coercion

-- | A simple "Data.Map" value used in several examples below.
--
-- @
--  test_table = M.fromList [ (1, "hello"), (2, "world") ]
-- @
test_table :: M.Map Int String
test_table = M.fromList [ (1, "hello"), (2, "world") ]

-- | This example shows how the @'Data.Map.Justified.member'@
-- function can be used to obtain a key whose type has been
-- augmented by a proof that the key is present in maps of a
-- certain type.
--
-- Where "Data.Map" may use a @'Maybe'@ type to ensure that
-- the user handles missing keys when performing a lookup,
-- here we use the @'Maybe'@ type to either tell the user
-- that a key is missing (by returning @'Nothing'@), or
-- actually give back evidence of the key's presence
-- (by returning @Just known_key@)
--
-- The @'Data.Map.Justified.withMap'@ function is used to
-- plumb a "Data.Map" @'Data.Map.Map'@ into a function that
-- expects a "Data.Map.Justified" @'Data.Map.Justified.Map'@.
-- In the code below, you can think of @table@ as @test_table@,
-- enhanced with the ability to use verified keys.
--
-- You can get from @table@ back to @test_table@ using the
-- function @'Data.Map.Justified.theMap'@.
--
-- @
--  example1 = withMap test_table $ \\table -> do
--
--    putStrLn "Is 1 a valid key?"
--    case member 1 table of
--      Nothing  -> putStrLn "  No, it was not found."
--      Just key -> putStrLn $ "  Yes, found key: " ++ show key
--
--    putStrLn "Is 5 a valid key?"
--    case member 5 table of
--      Nothing  -> putStrLn "  No, it was not found."
--      Just key -> putStrLn $ "  Yes, found key: " ++ show key
-- @
-- Output:
--
-- @
--  Is 1 a valid key?
--    Yes, found key: Key 1
--  Is 5 a valid key?
--    No, it was not found.
-- @
example1 :: IO ()
example1 = withMap test_table $ \table -> do

    putStrLn "Is 1 a valid key?"
    case member 1 table of
      Nothing  -> putStrLn "  No, it was not found."
      Just key -> putStrLn $ "  Yes, found key: " ++ show key

    putStrLn "Is 5 a valid key?"
    case member 5 table of
      Nothing  -> putStrLn "  No, it was not found."
      Just key -> putStrLn $ "  Yes, found key: " ++ show key

-- | Once you have obtained a verified key, how do you use it?
--
-- "Data.Map.Justified" has several functions that are similar
-- to ones found in "Data.Map" that operate over verified keys.
-- In this example, notice that we can extract values directly
-- from the map using @'Data.Map.Justified.lookup'@; since we already
-- proved that the key is present when we obtained a @Key ph k@
-- value, @'Data.Map.Justified.lookup'@ does not need to return a
-- @'Maybe'@ value.
--
-- @
--  example2 = withMap test_table $ \\table -> do
--  
--    case member 1 table of
--      Nothing  -> putStrLn "Sorry, I couldn't prove that the key is present."
--      Just key -> do
--        -- In this do-block, \'key\' represents the key 1, but carries type-level
--        -- evidence that the key is present. Lookups and updates can now proceed
--        -- without the possibility of error.
--        putStrLn ("Found key: " ++ show key)
--  
--        -- Note that lookup returns a value directly, not a \'Maybe\'!
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
example2 :: IO ()
example2 = withMap test_table $ \table -> do

  case member 1 table of
    Nothing  -> putStrLn "Sorry, I couldn't prove that the key is present."
    Just key -> do
      -- In this do-block, 'key' represents the key 1, but carries type-level
      -- evidence that the key is present. Lookups and updates can now proceed
      -- without the possibility of error.
      putStrLn ("Found key " ++ show key)

      -- Note that lookup returns a value directly, not a 'Maybe'!
      putStrLn ("Value for key: " ++ lookup key table)

      -- If you update an already-mapped value, the set of valid keys does
      -- not change. So the evidence that 'key' could be found in 'table'
      -- is still sufficient to ensure that 'key' can be found in the updated
      -- table as well.
      let table' = reinsert key "howdy" table
      putStrLn ("Value for key in updated map: " ++ lookup key table')

-- | It is a bit surprising to realize that a key of type @Key ph k@ can
-- be used to safely look up values in /any/ map of type @Map ph k v@,
-- not only the map that they key was originally found in!
--
-- This example makes use of that property to look up corresponding
-- elements of two /different/ (but related) tables, using the same
-- key evidence.
--
-- @
--  example3 = withMap test_table $ \\table -> do
--  
--    let uppercase = map toUpper
--        updated_table = fmap (reverse . uppercase) table
--    
--    for (keys table) $ \\key -> do
--      -- Although we are iterating over keys from the original table, they
--      -- can also be used to safely lookup values in the fmapped table.
--      -- Unlike (!) from Data.Map, Data.Map.Justified's (!) can not fail at runtime.
--      putStrLn ("In original table, " ++ show key ++ " maps to " ++ table ! key)
--      putStrLn ("In updated table,  " ++ show key ++ " maps to " ++ updated_table ! key)
--  
--    return ()
-- @
-- Output:
--
-- @
--  In original table, Key 1 maps to hello
--  In updated table,  Key 1 maps to OLLEH
--  In original table, Key 2 maps to world
--  In updated table,  Key 2 maps to DLROW
-- @
example3 :: IO ()
example3 = withMap test_table $ \table -> do

  let uppercase = map toUpper
      updated_table = fmap (reverse . uppercase) table
  
  forM_ (keys table) $ \key -> do
    -- Although we are iterating over keys from the original table, they
    -- can also be used to safely lookup values in the fmapped table.
    -- Unlike (!) from Data.Map, Data.Map.Justified's (!) can not fail at runtime.
    putStrLn ("In original table, " ++ show key ++ " maps to " ++ table ! key)
    putStrLn ("In updated table,  " ++ show key ++ " maps to " ++ updated_table ! key)

  return ()

-- | What if your set of keys can change over time?
--
-- If you were to insert a new key into a map, evidence that a key
-- exists is in the old map is no longer equivalent to evidence that
-- a key exists in the new map.
--
-- On the other hand, we know that if some @key@ exists in the old map,
-- then @key@ must still exist in the new map. So there should be a
-- way of "upgrading" evidence from the old map to the new. Furthermore,
-- we know that the key we just added must be in the new map.
--
-- The @'Data.Map.Justified.inserting'@ function inserts a value into a map
-- and feeds the new map into a continuation, along with the "upgrade" and
-- "new key" data.
--
-- @
--  example4 = withMap test_table $ \\table -> do
--    inserting 3 "NEW" table $ \\(newKey, upgrade, table') -> do
--      forM_ (keys table) $ \\key -> do
--        putStrLn (show key ++ " maps to " ++ table  ! key ++ " in the old table.")
--        putStrLn (show key ++ " maps to " ++ table' ! (upgrade key) ++ " in the new table.")
--      putStrLn ("Also, the new table maps " ++ show newKey ++ " to " ++ table' ! newKey)
-- @
-- Output:
--
-- @
--  Key 1 maps to hello in the old table.
--  Key 1 maps to hello in the new table.
--  Key 2 maps to world in the old table.
--  Key 2 maps to world in the new table.
--  Also, the new table maps Key 3 to NEW
-- @

example4 :: IO ()
example4 = withMap test_table $ \table -> do
  inserting 3 "NEW" table $ \(newKey, upgrade, table') -> do
    forM_ (keys table) $ \key -> do
      putStrLn (show key ++ " maps to " ++ table  ! key ++ " in the old table.")
      putStrLn (show key ++ " maps to " ++ table' ! (upgrade key) ++ " in the new table.")
    putStrLn ("Also, the new table maps " ++ show newKey ++ " to " ++ table' ! newKey)
    
-- | The next example uses a directed graph, defined by this adjacency list.
--
-- @
--  adjacencies = M.fromList [ (1, [2,3]), (2, [1,5,3]), (3, [4]), (4, [3, 1]), (5, []) ]
-- @
adjacencies :: M.Map Int [Int]
adjacencies = M.fromList [ (1, [2,3]), (2, [1,5,3]), (3, [4]), (4, [3, 1]), (5, []) ]

-- | Sometimes, the values in a map may include references back to keys
-- in the map. A canonical example of this is the adjacency map representation of
-- a directed graph, where each node maps to its list of immediate successors.
-- The type would look something like
--
-- @
--  type Digraphy node = M.Map node [node]
-- @
--
-- If you want to do a computation with a @Digraphy node@, you probably want each
-- of the neighbor nodes to have keys in the @Digraphy node@ map. That is, you
-- may really want
--
-- @
--  type Digraph ph node = Map ph node [Key ph node]
--                             \/\\           \/\\
--                             |            |
--                             +-----+------+
--                                   |
--   (each neighbor should carry a proof that they are also in the map)
-- @
-- You can do this via @'Data.Map.Justified.withRecMap'@, which converts each
-- key reference of type @k@ in your map to a verified key of type @'Key' ph k@.
--
-- But what if a referenced key really is missing from the map? @'Data.Map.Justified.withRecMap'@
-- returns an @'Either'@ value to represent failure; if a key is missing, then the
-- result will be a value of the form @'Left' problems@, where @problems@ is an explanation
-- of where the missing keys are.
--
-- @
--  example5 = do
--     -- Print out the nodes in a graph 
--     putStrLn ("Finding nodes in the directed graph " ++ show adjacencies)
--     trial adjacencies
--  
--     -- Now add the (non-present) node 6 as a target of an edge from node 4 and try again:
--     let adjacencies' = M.adjust (6:) 4 adjacencies
--     putStrLn ("Finding nodes in the directed graph " ++ show adjacencies')
--     trial adjacencies'
--     
--    where
--      trial adj = either showComplaint showNodes (withRecMap adj (map theKey . keys))
--      
--      showNodes nodes = putStrLn ("  Nodes: " ++ show nodes)
--  
--      showComplaint problems = do
--        putStrLn "  The following edges are missing targets:"
--        let badPairs = concatMap (\\(src, tgts) -> [ (src,tgt) | (tgt, Missing) <- tgts ]) problems
--        forM_ badPairs $ \\(src,tgt) -> putStrLn ("    " ++ show src ++ " -> " ++ show tgt)
-- @
-- Output:
--
-- @
--  Finding nodes in the directed graph fromList [(1,[2,3]),(2,[1,5,3]),(3,[4]),(4,[3,1]),(5,[])]
--    Nodes: [1,2,3,4,5]
--  Finding nodes in the directed graph fromList [(1,[2,3]),(2,[1,5,3]),(3,[4]),(4,[6,3,1]),(5,[])]
--    The following edges are missing targets:
--      4 -> 6
-- @
example5 :: IO ()
example5 = do
   -- Print out the nodes in a graph 
   putStrLn ("Finding nodes in the directed graph " ++ show adjacencies)
   trial adjacencies

   -- Now add the (non-present) node 6 as a target of an edge from node 4 and try again:
   let adjacencies' = M.adjust (6:) 4 adjacencies
   putStrLn ("Finding nodes in the directed graph " ++ show adjacencies')
   trial adjacencies'
   
  where
    trial adj = either showComplaint showNodes (withRecMap adj (map theKey . keys))
    
    showNodes nodes = putStrLn ("  Nodes: " ++ show nodes)

    showComplaint problems = do
      putStrLn "  The following edges are missing targets:"
      let badPairs = concatMap (\(src, tgts) -> [ (src,tgt) | (tgt, Missing) <- tgts ]) problems
      forM_ badPairs $ \(src,tgt) -> putStrLn ("    " ++ show src ++ " -> " ++ show tgt)
