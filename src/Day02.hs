module Day02 (parseInput, part1, part2) where

import Data.List
import Data.List.Split

parseInput :: String -> [[Int]]
parseInput = map (map read . splitOn " ") . lines

validReport :: [Int] -> Bool
validReport = ((||) <$> diffsWithin 1 3 <*> diffsWithin (-3) (-1)) . (zip <*> tail)

diffsWithin :: Int -> Int -> [(Int, Int)] -> Bool
diffsWithin lo hi = all (\(x, y) -> lo <= y - x && y - x <= hi)

removeOne :: [Int] -> [[Int]]
removeOne input = filter ((== n - 1) . length) $ subsequences input
  where
    n = length input

part1 :: [[Int]] -> Int
part1 = length . filter validReport

part2 :: [[Int]] -> Int
part2 = length . filter (\r -> validReport r || any validReport (removeOne r))
