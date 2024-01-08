onlyDigits :: String -> Bool
onlyDigits "" = False
onlyDigits (x:xs)
  | (x >= '0') && (x <= '9') && xs == "" = True
  | (x >= '0') && (x <= '9') = onlyDigits xs
  | otherwise = False

