{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Data.Map.JustifiedSpec (main, spec) where

import Prelude hiding (lookup)

import Test.Hspec
import Test.QuickCheck
import Test.ShouldNotTypecheck

import Data.Map.Justified
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = hspec spec

letters :: M.Map Char Int
letters = M.fromList (Prelude.zip ['a'..'z'] [1..])

newtype AlphaNum = AlphaNum Char deriving Show
instance Arbitrary AlphaNum where
  arbitrary = elements (map AlphaNum $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
  
spec :: Spec
spec = do

  describe "validated keys" $ do
    
    it "can be obtained iff a key is present" $ property $
      \(AlphaNum c) -> isJust (M.lookup c letters) == withMap letters (isJust . member c)

    it "will access the expected value" $ do
      map (\k -> fromJust $ M.lookup k letters) (M.keys letters)
      `shouldBe`
      withMap letters (\m -> map (`lookup` m) (keys m))
      
    it "can degrade to the original key" $ do
      M.keys letters `shouldBe` withMap letters (map theKey . keys)

    it "can still be used in an adjusted map" $ do
      withMap letters (\m -> let (k:k':_) = keys m in
                                 (lookup k (reinsert k 17 m),
                                  lookup k (reinsert k' 17 m)))
      `shouldBe` (17,1)
      
    it "can not escape `withMap`" $ shouldNotTypecheck $
      withMap letters (head keys)

    it "can not be used in unrelated maps" $ shouldNotTypecheck $
      withMap letters (\m ->
         let k = head (keys m) in withMap letters (lookup k))