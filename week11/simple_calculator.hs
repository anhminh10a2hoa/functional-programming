calculate :: [String] -> [String]
calculate list = map calc list


calc :: String -> String
calc str =
  case words str of    -- words "3 + 5" -> ["3", "+", "5"]
    [a, operation, b] ->
      case (readMaybe a, readMaybe b) of 
        (Just x, Just y) ->
          case operation of 
            "+" -> show (x + y)
            "-" -> show (x - y)
            "*" -> show (x * y)
            _   -> "I cannot calculate that"
        _ -> "I cannot calculate that"
    _ -> "I cannot calculate that"

readMaybe :: (Read a) => String -> Maybe a
readMaybe str = case reads str of
                    [(x,"")] -> Just x
                    _ -> Nothing