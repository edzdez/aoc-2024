module Day08 where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Debug.Trace

type Point = (Int, Int)

inBounds :: Int -> Int -> Int -> Bool
inBounds lo hi = (&&) <$> (>= lo) <*> (<= hi)

pointInBounds :: Point -> Point -> Bool
pointInBounds (n, m) (x, y) = inBounds 1 n x && inBounds 1 m y

minMax :: Int -> Int -> (Int, Int)
minMax a b
  | a < b = (a, b)
  | otherwise = (b, a)

parseInput :: String -> (M.Map Char [Point], Int, Int)
parseInput input =
  let rows = lines input
      n = length $ head rows
      m = length rows
      nodes =
        foldr
          ( \(x, y, c) acc ->
              M.alter (Just . maybe [(x, y)] ((x, y) :)) c acc
          )
          M.empty
          $ concat
          $ zipWith
            ( \y r ->
                map (\(x, c) -> (x, y, c)) $
                  filter ((/= '.') . snd) $
                    zip [1 ..] r
            )
            [1 ..]
            rows
   in (nodes, n, m)

antinodes :: [Point] -> S.Set Point
antinodes [] = S.empty
antinodes ((x, y) : ps) =
  S.union (antinodes ps) $
    S.fromList $
      concatMap
        ( \(x', y') ->
            let dx = abs (x - x')
                dy = abs (y - y')
             in [ (x + sign x x' dx, y + sign y y' dy),
                  (x' + sign x' x dx, y' + sign y' y dy)
                ]
                  ++ if dx `rem` 3 == 0 && dy `rem` 3 == 0
                    then
                      [ (x + sign x' x dx `div` 3, y + sign y' y dy `div` 3),
                        (x + sign x x' dx `div` 3, y + sign y y' dy `div` 3)
                      ]
                    else []
        )
        ps
  where
    sign a b c = if a < b then (-c) else c

part1 :: (M.Map Char [Point], Int, Int) -> Int
part1 (nodes, n, m) =
  S.size $
    M.foldr
      ( \ps s' ->
          S.union s' $
            S.filter (pointInBounds (n, m)) $
              antinodes ps
      )
      S.empty
      nodes

resonances :: Int -> Int -> [Point] -> S.Set Point
resonances _ _ [] = S.empty
resonances n m ((x, y) : ps) =
  S.union (resonances n m ps) $
    S.fromList $
      concatMap
        ( \(x', y') ->
            if x == x'
              then map (x,) [1 .. n]
              else
                if y == y'
                  then map (,y) [1 .. m]
                  else
                    let dx = x' - x
                        dy = y' - y
                        dx' = dx `div` gcd dx dy
                        dy' = dy `div` gcd dx dy
                        up = takeWhile (pointInBounds (n, m)) $ iterate (\(x'', y'') -> (x'' + dx', y'' + dy')) (x, y)
                        down = takeWhile (pointInBounds (n, m)) $ iterate (\(x'', y'') -> (x'' - dx', y'' - dy')) (x, y)
                     in up ++ down
        )
        ps

part2 :: (M.Map Char [Point], Int, Int) -> Int
part2 (nodes, n, m) =
  let s =
        M.foldr
          (\ps s' -> S.union s' $ resonances n m ps)
          S.empty
          nodes
   in S.size s
