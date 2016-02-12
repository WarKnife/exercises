module Exercises.FindMissingNumbers (findMissingPair) where

findMissingPair :: [Integer] -> Maybe (Integer, Integer)
findMissingPair [] = Nothing
findMissingPair s = findMP (1, []) s
  where
    findMP (_, [a,b])       _            = Just (a,b)
    findMP _                []           = Nothing
    findMP (i, missings)    (current:xs)
      | i == current    = findMP (nextI, missings) xs
      | otherwise       = if nextI == current
                            then findMP (succ nextI,
                                         missings++[i]) xs
                            else Just   (i, nextI)
      where nextI = succ i

