import Data.List
import Data.List.Split

main = do
  input <- chunksOf (25*6) . init <$> readFile "8.txt"
  print (length input, input)
  let (c, l) = minimum . map (\x -> (length (filter (=='0') x), x)) $ input
  print (c, l)
  let answer = length ((filter (=='1')) l) * length ((filter (=='2')) l)
  print answer
  let vs =  map findValue $ transpose input
  mapM_ print $ paintPixels vs

findValue :: [Char] -> Char 
findValue ('2':cs) = findValue cs
findValue (c:_)    = c

paintPixel :: Char -> Char
paintPixel '0' = ' ' 
paintPixel '1' = 'O'

paintPixels :: String -> [String]
paintPixels = (map . map) paintPixel . chunksOf 25
