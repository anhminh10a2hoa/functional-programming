commonSubstring :: String -> String -> String
commonSubstring [] [] = ""
commonSubstring _ [] = ""
commonSubstring [] _ = ""
commonSubstring s1 s2 = foldl findSubstring [] s1
  where findSubstring substring x 
          | x `elem` s2 = x: substring
          | otherwise = substring


clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters func distance list = foldl (\acc x -> (filter (\y -> ( func x y ) <= distance) list): acc ) [] list

clusters2 :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters2 func distance list = foldr filterByDistance [] list
  where filterByDistance elem acc = filter (\y -> ( func elem y ) <= distance) list : acc