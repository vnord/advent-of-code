import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe

pair :: String -> (Char, Int)
pair (x:xs) = (x, read xs)

main = do
 (w1:w2:_) <-  (map.map) pair . map (splitOn ",") . lines <$> readFile "3.txt"
 let t1 = Map.fromList $ trace w1 [((0,0),0)]
 let t2 = Map.fromList $ trace w2 [((0,0),0)]
 let i = Map.intersection t1 t2
 print $ head $ filter (/= 0) . Map.keys $ Map.mapKeys (\(x,y) -> 
    (fromJust (Map.lookup (x,y) t1)) + (fromJust (Map.lookup (x,y) t2))) i

trace :: [(Char, Int)] -> [((Int,Int),Int)] -> [((Int,Int),Int)]
trace [] xs = xs
trace ((_,0):ds) xs = trace ds xs
trace (('R',val):ds) xs@(((x,y),c):_) = trace (('R',val-1):ds) (((x+1,y),c+1):xs)
trace (('L',val):ds) xs@(((x,y),c):_) = trace (('L',val-1):ds) (((x-1,y),c+1):xs)
trace (('U',val):ds) xs@(((x,y),c):_) = trace (('U',val-1):ds) (((x,y+1),c+1):xs)
trace (('D',val):ds) xs@(((x,y),c):_) = trace (('D',val-1):ds) (((x,y-1),c+1):xs)
