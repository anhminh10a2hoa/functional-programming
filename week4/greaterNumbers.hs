nextIsGreater :: [Int] -> [Int]
nextIsGreater [] = []
nextIsGreater [x] = []
nextIsGreater (x:y:xs)
  | x < y = x : nextIsGreater (y:xs)
  | otherwise = nextIsGreater (y:xs)


