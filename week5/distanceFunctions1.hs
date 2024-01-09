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


digits = ['0'..'9']

countOfNotDigits :: String -> Int
countOfNotDigits [] = 0
countOfNotDigits (x:str)
  | elem x digits = countOfNotDigits str
  | otherwise = 1 + countOfNotDigits str


distance2 :: String -> String -> Float
distance2 [] [] = 0
distance2 s1 s2 = (fromIntegral ( (countOfNotDigits s1) + (countOfNotDigits s2) )) / (fromIntegral ( length s1 + length s2))

