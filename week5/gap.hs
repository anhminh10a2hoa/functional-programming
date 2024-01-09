gap :: (Char, Char) -> Int -> String -> Int
gap (_, _) _ [] = 0
gap (_, _) _ [x] = 0
gap (_, _) i string
  | i >= length string = 0
gap (x, y) i (z:string)
  | i >= length string = 0
  | x == z && string!!i == y = 1 + gap (x, y) i string
  | otherwise = gap (x, y) i string


checkChar :: Char -> (String -> Bool)
checkChar char = \str -> elem char str
