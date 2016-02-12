module Exercises.FindMissingNumbersSpec (main, spec) where

import Test.Hspec
import Data.List (delete)
import Test.QuickCheck

import Exercises.FindMissingNumbers

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "findMissingPair" $ do
    context "when input sequence contains only one or no missed numbers" $
      it "finds nothing" $ do
        findMissingPair [1..10]     `shouldBe` Nothing
        findMissingPair [1,2,3,5,6] `shouldBe` Nothing
    it "finds first two numbers that are missed in sequence from 1 to 1000000" $
      property $
        \(NonNegative a) (NonNegative b) -> (0 < a && a < b  && b < 1000000)
        ==>
          findMissingPair (delete a . delete b $ [1..1000000])
          ==
          Just (a, b)
