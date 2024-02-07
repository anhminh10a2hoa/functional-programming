clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters func distance list = foldl (\acc x -> (filter (\y -> ( func x y ) <= distance) list): acc ) [] list

clusters2 :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters2 func distance list = foldr filterByDistance [] list
  where filterByDistance elem acc = filter (\y -> ( func elem y ) <= distance) list : acc

distance1 :: String -> String -> Float
distance1 [] [] = 0
distance1 str [] = fromIntegral ( length str + 0) / fromIntegral ( length str  + 0)
distance1 [] str = fromIntegral ( 0 + length str) / fromIntegral ( 0 + length str )
distance1 s1 s2 = (fromIntegral ( (helper1 s1 s2) + (helper1 s2 s1) )) / (fromIntegral ( length s1 + length s2))

helper1 :: String -> String -> Int
helper1 [] [] = 0
helper1 [] _ = 0
helper1 (x:s1) s2 
  | elem x s2 = helper1 s1 s2 
  | otherwise = 1 + helper1 s1 s2 