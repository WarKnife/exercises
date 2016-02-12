module Exercises.Factorial (getFactOf) where

getFactOf :: Integer -> Integer
getFactOf n
  | n < 0     = error "Only non-negative integers allowed as input"
  | otherwise = computeFact n
        where
          computeFact 0 = 1
          computeFact n = product [1..n]
