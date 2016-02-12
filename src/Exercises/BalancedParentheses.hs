module Exercises.BalancedParentheses (isValid) where

isValid :: String -> Bool
isValid xs = isBalanced xs []
    where
      isBalanced []     x       = null x
      isBalanced (c:xs) ys
        | Just d <- lookup c par = isBalanced xs (d:ys)
            where par = [('(', ')')]
      isBalanced _      []      = False
      isBalanced (x:xs) (y:ys)  = x == y && isBalanced xs ys
