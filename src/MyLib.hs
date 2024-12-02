module MyLib (test) where

test :: String -> (String -> a) -> (a -> Int) -> IO ()
test filename parseInput solution = do
  contents <- readFile $ "inputs/" ++ filename
  let input = parseInput contents
  print $ solution input
