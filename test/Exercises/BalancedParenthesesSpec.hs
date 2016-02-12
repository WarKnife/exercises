module Exercises.BalancedParenthesesSpec (main, spec) where

import Test.Hspec

import Exercises.BalancedParentheses

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "isValid" $ do
    context "when input string is empty" $
      it "valid" $
        isValid "" `shouldBe` True
    context "when input string contains balanced parentheses" $
      it "valid" $ do
        isValid "()" `shouldBe` True
        isValid "((()))" `shouldBe` True
        isValid "()()()" `shouldBe` True
        isValid "()(())((()))" `shouldBe` True
    context "when input string contains not balanced parentehses" $
      it "not valid" $ do
        isValid "(" `shouldBe` False
        isValid ")" `shouldBe` False
        isValid "(()" `shouldBe` False
        isValid ")))" `shouldBe` False
        isValid "((()))()(" `shouldBe` False
