module Day04 where

import Data.List.Split
import Data.Matrix qualified as M
import GHC.Base

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(i, j) | i <- xs, j <- ys]

parseInput :: String -> M.Matrix Char
parseInput = M.fromLists . lines

countXmasInLine :: String -> Int
countXmasInLine = length . filter (== "XMAS") . divvy 4 1

takeDiagonal :: M.Matrix a -> Int -> [a]
takeDiagonal mat i =
  let n = M.nrows mat
   in zipWith (curry (mat M.!)) [1 .. n - i + 1] (enumFrom i)

diagonals :: M.Matrix a -> [[a]]
diagonals mat =
  let n = M.nrows mat
   in reverse (takeDiagonal' mat [1 .. n]) ++ takeDiagonal' (M.transpose mat) [2 .. n]
  where
    takeDiagonal' mat' = map (takeDiagonal mat')

part1 :: M.Matrix Char -> Int
part1 board =
  let southEast = diagonals board
      southWest = diagonals $ M.fromLists $ map reverse $ M.toLists board
   in sum $
        map
          ((+) <$> countXmas id <*> countXmas reverse)
          [ M.toLists board,
            M.toLists $ M.transpose board,
            southEast,
            southWest
          ]
  where
    countXmas f = sum . map (countXmasInLine . f)

part2 :: M.Matrix Char -> Int
part2 board =
  length $
    filter (isXMas . \(a, b) -> M.submatrix a (a + 2) b (b + 2) board) $
      join cartesianProduct [1 .. M.nrows board - 2]
  where
    isXMas c =
      let southEast = takeDiagonal c 1
          southWest = takeDiagonal (M.fromLists $ map reverse $ M.toLists c) 1
       in isMas southWest && isMas southEast
      where
        isMas = (||) <$> (== "MAS") <*> (== "SAM")
