points :: Int -> [(Int, Int)]
points a = reverse [ (x, y) | x <- [-a..a], y <- [-a..a], (abs (0 - x) + abs (0 - y)) <= a ]