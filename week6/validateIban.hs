digits = ['0'..'9']

-- 'F' = 15, 'I' = 18
moveAndReplace iban = drop 4 iban ++ "15" ++ "18" ++ (drop 2(take 4 iban))


validate :: String -> Bool
validate [] = False
validate string
  | length string /= 18 = False
validate (f:s:rest)
  | f /= 'F' && s /= 'I' = False
  | length (filter (\x -> elem x digits) rest) /= 16 = False
  | read (moveAndReplace (f:s:rest)) `mod` 97 == 1 = True
  | otherwise = False
