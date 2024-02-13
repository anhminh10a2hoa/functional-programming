-- Define a helper function to find the index of a character in a string
elemIndex' :: Eq a => a -> [a] -> Maybe Int
elemIndex' _ [] = Nothing
elemIndex' x (y:ys)
  | x == y = Just 0
  | otherwise = fmap (+ 1) (elemIndex' x ys)

-- Function to find the longest common substring using the elemIndex' helper
commonSubstring :: String -> String -> String
commonSubstring [] _ = []
commonSubstring _ [] = []
commonSubstring (first:rest) str2 = 
    case elemIndex' first str2 of
        Just index -> 
            first : commonSubstring rest (drop (index + 1) str2)
        Nothing -> 
            commonSubstring rest str2

-- Function to find the longest common substring without using elemIndex'
commonSubstring2 :: String -> String -> String
commonSubstring2 [] _ = []
commonSubstring2 _ [] = []
commonSubstring2 (first:rest) str2 =
  if first `elem` str2
    then first : commonSubstring2 rest (dropWhile (/= first) str2)
    else commonSubstring2 rest str2
