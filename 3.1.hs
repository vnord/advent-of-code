import Data.List.Split
import qualified Data.Set as Set

pair :: String -> (Char, Int)
pair (x:xs) = (x, read xs)

main = do
 (w1:w2:_) <-  (map.map) pair . map (splitOn ",") . lines <$> readFile "3.txt"
 let t1 = Set.fromList $ trace w1 [(0,0)]
 let t2 = Set.fromList $ trace w2 [(0,0)]
 print $ Set.findMin . Set.map dist $ Set.intersection t1 t2

dist :: (Int, Int) -> Int
dist (0,0) = 50000
dist (x,y) = abs x + abs y

trace :: [(Char, Int)] -> [(Int,Int)] -> [(Int, Int)]
trace [] xs = xs
trace ((_,0):ds) xs = trace ds xs
trace (('R',val):ds) xs@((x,y):_) = trace (('R',val-1):ds) ((x+1,y):xs)
trace (('L',val):ds) xs@((x,y):_) = trace (('L',val-1):ds) ((x-1,y):xs)
trace (('U',val):ds) xs@((x,y):_) = trace (('U',val-1):ds) ((x,y+1):xs)
trace (('D',val):ds) xs@((x,y):_) = trace (('D',val-1):ds) ((x,y-1):xs)
