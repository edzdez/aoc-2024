module Day06 where

import Control.Monad qualified as M
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as S
import Debug.Trace

type Point = (Int, Int)

type Graph = (S.Set Point, Point)

type State = (Graph, Point)

data Direction = North | South | East | West
  deriving (Ord, Eq, Show)

parseInput :: String -> (Graph, Point)
parseInput input =
  let rows = zip [1 ..] $ lines input
      guard =
        head $
          concatMap
            (\(y, row) -> [(x + 1, y) | x <- L.elemIndices '^' row])
            rows
      obstacles =
        S.fromList $
          concatMap
            (\(y, row) -> [(x + 1, y) | x <- L.elemIndices '#' row])
            rows
   in ((obstacles, (length $ snd $ head rows, length rows)), guard)

next :: Point -> Direction -> Point
next (x, y) North = (x, y - 1)
next (x, y) South = (x, y + 1)
next (x, y) East = (x + 1, y)
next (x, y) West = (x - 1, y)

rotate90 :: Direction -> Direction
rotate90 North = East
rotate90 East = South
rotate90 South = West
rotate90 West = North

addObstacle :: Point -> Graph -> Graph
addObstacle p (obstacles, dim) = (S.insert p obstacles, dim)

inBounds :: Point -> Point -> Bool
inBounds (x, y) (m, n) = 1 <= x && x <= m && 1 <= y && y <= n

simulate' :: Graph -> Point -> Direction -> S.Set (Point, Direction) -> Either (Point, Direction, S.Set (Point, Direction)) Int
simulate' (obstacles, (m, n)) (x, y) dir visited
  | not $ inBounds (x, y) (m, n) = Right 0
  | ((x, y), dir) `S.member` visited = Right 1
  | otherwise =
      let isObstacle = next (x, y) dir `S.member` obstacles
          dir' = if isObstacle then rotate90 dir else dir
          nxt = next (x, y) dir'
       in do Left (nxt, dir', S.insert ((x, y), dir) visited)

simulate :: Graph -> Point -> Direction -> [(Point, Direction)]
simulate = simulateAux [] S.empty
  where
    simulateAux acc visited graph guard dir =
      let nxt = simulate' graph guard dir visited
       in case nxt of
            Right 0 -> acc
            Right 1 -> []
            Left (guard', dir', visited') -> simulateAux ((guard, dir) : acc) visited' graph guard' dir'
            _ -> undefined

part1 :: State -> Int
part1 (graph, guard) = S.size $ S.fromList $ map fst $ simulate graph guard North

foo :: State -> S.Set Point
foo (graph, guard) = S.filter (\p -> null $ simulate (addObstacle p graph) guard North) $ S.fromList $ map fst $ simulate graph guard North

part2 :: State -> Int
part2 (graph, guard) =
  let reach = S.fromList $ map fst $ simulate graph guard North
   in length $ S.filter (\p -> null $ simulate (addObstacle p graph) guard North) reach
