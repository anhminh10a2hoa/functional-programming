distance3 :: String -> String -> Float
distance3 x y = fromIntegral $ abs $ length x - length y

distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
distanceFilter func distance str list = filter (\x -> (func str x) <=distance) list