module Exercises.FactorialSpec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Exercises.Factorial

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "getFactOf" $ do
    context "when input is negative number" $
      it "throws exception" $
        evaluate (getFactOf (-1)) `shouldThrow` anyErrorCall
    context "when input is non-negative number" $ do
      it "factorial of 0 must equal 1" $
        getFactOf 0 `shouldBe` 1
      it "factorials sequance of numbers from 1 to 10 evaluates correctly" $
        map getFactOf [1..10] `shouldBe`
        [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]
