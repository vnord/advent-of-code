monotonicDigits :: Int -> Bool
monotonicDigits = monotonicString . show

monotonicString :: String -> Bool
monotonicString (x:y:xs) = x <= y && monotonicString (y:xs)
monotonicString (_:[]) = True

hasDoubleDigits :: Int -> Bool
hasDoubleDigits = hasDoubleChars . show

hasDoubleChars :: String -> Bool
hasDoubleChars (x:y:xs) = (x == y) || hasDoubleChars (y:xs)
hasDoubleChars (_:[]) = False

part1 :: Int
part1 = length $ filter hasDoubleDigits $ filter monotonicDigits [138241 .. 674034]
