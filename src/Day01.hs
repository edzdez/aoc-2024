module Day01 where

import Data.List
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Maybe

parseInput :: String -> [[Int]]
parseInput = transpose . map parseLine . lines
  where
    parseLine = map (read :: String -> Int) . splitOn "   "

part1 :: [[Int]] -> Int
part1 = sum . map (abs . foldr (-) 0) . transpose . map sort

part2 :: [[Int]] -> Int
part2 [xs, ys] = sum $ map (\n -> n * M.findWithDefault 0 n freqs) xs
  where
    freqs = foldr (M.alter (\v -> Just $ (1 :: Int) + fromMaybe 0 v)) M.empty ys
part2 _ = undefined
