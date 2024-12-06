module Day06 (parseInput, part1, part2) where

import Data.List qualified as L
import Data.Set qualified as S

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

simulate' :: Graph -> Point -> Direction -> S.Set (Point, Direction) -> Maybe (Point, Direction, S.Set (Point, Direction))
simulate' (obstacles, (m, n)) (x, y) dir visited
  | not $ inBounds (x, y) (m, n) = Nothing
  | ((x, y), dir) `S.member` visited = Nothing
  | otherwise =
      let isObstacle = next (x, y) dir `S.member` obstacles
          dir' = if isObstacle then rotate90 dir else dir
          nxt = next (x, y) dir'
       in Just (nxt, dir', S.insert ((x, y), dir) visited)

simulate :: Graph -> Point -> Direction -> [(Point, Direction)]
simulate = simulateAux [] S.empty
  where
    simulateAux acc visited graph guard dir =
      let nxt = simulate' graph guard dir visited
       in case nxt of
            Nothing -> acc
            Just (guard', dir', visited') -> simulateAux ((guard, dir) : acc) visited' graph guard' dir'

hasCycle :: Point -> Graph -> Bool
hasCycle guard (obstacles, (m, n)) = hasCycle' guard North S.empty
  where
    hasCycle' :: Point -> Direction -> S.Set (Point, Direction) -> Bool
    hasCycle' point dir visited
      | (point, dir) `S.member` visited = True
      | otherwise =
          let nextBunch = scanl (\_ -> flip next dir) point nextBunch
              nextObstacle =
                last $
                  takeWhile
                    ((&&) <$> flip inBounds (m, n) <*> flip S.notMember obstacles)
                    nextBunch
              dir' = rotate90 dir
              visited' = S.insert (point, dir) visited
           in next nextObstacle dir `S.member` obstacles
                && hasCycle' nextObstacle dir' visited'

part1 :: State -> Int
part1 (graph, guard) = S.size $ S.fromList $ map fst $ simulate graph guard North

part2 :: State -> Int
part2 (graph, guard) =
  let reach = S.fromList $ map fst $ simulate graph guard North
   in length $ S.filter (hasCycle guard . flip addObstacle graph) $ S.delete guard reach
