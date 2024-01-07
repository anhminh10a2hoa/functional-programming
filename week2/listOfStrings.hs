headOrLast :: [String] -> Char -> [String]
headOrLast inputList x = [ y | y <- inputList, length y > 0, head y == x || last y == x ]
