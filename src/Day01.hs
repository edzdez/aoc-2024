module Day01 where

import Data.List
import Data.List.Split
import Data.Map.Strict qualified as M

parseInput :: String -> [[Int]]
parseInput = transpose . map parseLine . lines
  where
    parseLine = map (read :: String -> Int) . splitOn "   "

part1 :: [[Int]] -> Int
part1 = sum . map (abs . foldr (-) 0) . transpose . map sort

part2 :: [[Int]] -> Int
part2 [xs, ys] = sum $ map (\n -> n * M.findWithDefault 0 n freqs) xs
  where
    freqs = foldr (M.alter (Just . maybe 1 (+ 1))) M.empty ys
part2 _ = undefined
