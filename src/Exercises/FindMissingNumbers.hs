module Exercises.FindMissingNumbers (findMissingPair) where

findMissingPair :: [Integer] -> Maybe (Integer, Integer)
findMissingPair [] = Nothing
findMissingPair ls = findMP (1, []) ls
  where
    findMP (i, [a,b]) _      = Just (a,b)
    findMP _          []     = Nothing
    findMP (i, ys)    (x:xs)
      | i == x    = findMP (succ i, ys) xs
      | otherwise = if succ i == x
                      then findMP (i+2, ys++[i]) xs
                      else Just   (i, succ i)
