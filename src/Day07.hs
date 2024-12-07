module Day07 (parseInput, part1, part2) where

import Data.List.Split

type Test = (Integer, [Integer])

parseInput :: String -> [Test]
parseInput =
  map
    ( \line ->
        let (a, b) = break (== ':') line
         in (read a, map read $ splitOn " " $ drop 2 b)
    )
    . lines

possiblyTrue :: Bool -> Test -> Bool
possiblyTrue p2 (res, nums) = possiblyTrue' res $ reverse nums
  where
    possiblyTrue' target [] = target == 0
    possiblyTrue' target (x : xs)
      | target <= 0 = False
      | otherwise =
          possiblyTrue' (target - x) xs
            || (target `rem` x == 0 && possiblyTrue' (target `div` x) xs)
            || (p2 && target `rem` msk == x && possiblyTrue' (target `div` msk) xs)
      where
        msk = 10 ^ length (show x)

part1 :: [Test] -> Integer
part1 = sum . map fst . filter (possiblyTrue False)

part2 :: [Test] -> Integer
part2 = sum . map fst . filter (possiblyTrue True)
