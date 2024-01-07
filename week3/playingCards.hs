credits :: (Char, Int) -> (Char, Int) -> Int
credits (c1, n1) (c2, n2)
  | (c1 == s && n1 == 14) || (c2 == s && n2 == 14) = 14
  | (c1 == c2 ) && (n1 +1 == n2 || n2 +1 == n1) = 8
  | (n1 == n2 ) = 6
  | (n1 +1 == n2 || n2 +1 == n1) = 4
  | (c1 == c2 ) = 2
  | otherwise = 0
  where (s, h, c, d) = ('s','h', 'c', 'd')

