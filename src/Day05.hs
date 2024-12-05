module Day05 (parseInput, part1, part2) where

import Data.List qualified as L
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

type Update = [Int]

type Graph = M.Map Int (S.Set Int)

parseInput :: String -> (Graph, [Update])
parseInput input =
  let orderUpdates = splitWhen null $ lines input
      order = map (\line -> let xs = splitOn "|" line in (read $ head xs, read $ xs !! 1)) $ head orderUpdates
      orderMap = foldr (\(x, y) -> M.alter (Just . maybe (S.singleton x) (S.insert x)) y) M.empty order
      updates = map (map read . splitOn ",") $ orderUpdates !! 1
   in (orderMap, updates)

cmp :: Graph -> Int -> Int -> Ordering
cmp adj a b
  | b `elem` fromMaybe S.empty (M.lookup a adj) = GT
  | a `elem` fromMaybe S.empty (M.lookup b adj) = LT
  | otherwise = EQ

part1 :: (Graph, [Update]) -> Int
part1 (order, updates) =
  let validUpdates = filter ((==) <*> L.sortBy (cmp order)) updates
   in sum $ map (\xs -> xs !! (length xs `div` 2)) validUpdates

part2 :: (Graph, [Update]) -> Int
part2 (order, updates) =
  let invalidUpdates = filter ((/=) <*> L.sortBy (cmp order)) updates
      reorderedUpdates = map (L.sortBy $ cmp order) invalidUpdates
   in sum $ map (\xs -> xs !! (length xs `div` 2)) reorderedUpdates
