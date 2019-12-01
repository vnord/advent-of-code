main = interact $ show . sum . map fuel . map read . lines

fuel :: Integer -> Integer
fuel x | f x <= 0 = 0
       | otherwise = f x + fuel (f x)
  where 
    f x = (x `div` 3) - 2
