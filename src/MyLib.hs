module MyLib (test) where

test :: (Show b) => String -> (String -> a) -> (a -> b) -> IO ()
test filename parseInput solution = do
  contents <- readFile $ "inputs/" ++ filename
  let input = parseInput contents
  print $ solution input
