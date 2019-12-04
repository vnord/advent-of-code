import Data.List

monotonicDigits :: Int -> Bool
monotonicDigits = monotonicString . show

monotonicString :: String -> Bool
monotonicString (x:y:xs) = x <= y && monotonicString (y:xs)
monotonicString (_:[]) = True

hasDoubleDigits :: Int -> Bool
hasDoubleDigits = hasDoubleChars . show

hasDoubleDigits' :: Int -> Bool
hasDoubleDigits' = hasDoubleChars' . show

hasDoubleChars :: String -> Bool
hasDoubleChars (x:y:xs) = x == y || hasDoubleChars (y:xs)
hasDoubleChars (_:[]) = False

hasDoubleChars' :: String -> Bool
hasDoubleChars' = not . null . filter (\x -> length x == 2) . group

part1 :: Int
part1 = length $ filter hasDoubleDigits
               $ filter monotonicDigits [138241 .. 674034]

part2 :: Int
part2 = length $ filter hasDoubleDigits' 
               $ filter monotonicDigits [138241 .. 674034]
