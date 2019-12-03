import Data.List
import Data.List.Split
import Control.Lens
import Control.Monad.State
import Data.Maybe
import qualified Data.Set as Set

pair :: String -> (Char, Int)
pair (x:xs) = (x, read xs)

data Line = HLine HLine | VLine VLine
  deriving Show

data HLine = H {
    _y      :: Int
 ,  _startX :: Int
 ,  _endX   :: Int
} deriving Show

data VLine = V {
    _x      :: Int
 ,  _startY :: Int
 ,  _endY   :: Int
} deriving Show

main = do
 (w1:w2:_) <-  (map.map) pair . map (splitOn ",") . lines <$> readFile "3.txt"
 let t1 = trace w1 [(0,0)]
 let t2 = trace w2 [(0,0)]
 let l1 = makeLines t1
 let l2 = makeLines t2
 let c = sort $ map dist . map fromJust . filter isJust $ crossings l1 l2
 print c
 -- let t1 = trace e1 [(0,0)]
 -- let t2 = trace e2 [(0,0)]
 -- let l1 = makeLines t1
 -- let l2 = makeLines t2
 -- let c' = map fromJust . filter isJust $ crossings l1 l2
 -- let c = sort . map dist . map fromJust . filter isJust $ crossings l1 l2
 -- print c'
 -- print c

crossing :: Line -> Line -> Maybe (Int, Int)
crossing (HLine (H y startX endX)) (VLine (V x startY endY)) =
  if (abs startX <=  abs x && abs x <=  abs endX)
      && (abs startY <= abs  y &&  abs y <= abs endY)
  then Just (x, y)
  else Nothing
crossing _ _ = Nothing

crossings :: [Line] -> [Line] -> [Maybe (Int,Int)]
crossings l1s l2s = [crossing l1 l2 | l1 <- l1s, l2 <-l2s]

dist :: (Int, Int) -> Int
dist (0,0) = 50000
dist (x,y) = x + y


trace :: [(Char, Int)] -> [(Int,Int)] -> [(Int, Int)]
trace (('R',val):ds) xs@((x,y):_) = trace ds ((x+val,y):xs)
trace (('L',val):ds) xs@((x,y):_) = trace ds ((x-val,y):xs)
trace (('U',val):ds) xs@((x,y):_) = trace ds ((x,y+val):xs)
trace (('D',val):ds) xs@((x,y):_) = trace ds ((x,y-val):xs)
trace [] xs = xs


makeLines :: [(Int,Int)] -> [Line]
makeLines ((x1,y1):(x2,y2):xs) | x1 == x2 && y1 <= y2 = (VLine $ V x1 y1 y2) : makeLines ((x2,y2):xs)
                               | x1 == x2 && y1 >  y2 = (VLine $ V x1 y2 y1) : makeLines ((x2,y2):xs)
                               | y1 == y2 && x1 <= x2 = (HLine $ H y1 x1 x2) : makeLines ((x2,y2):xs)
                               | y1 == y2 && x1 >  x2 = (HLine $ H y1 x2 x1) : makeLines ((x2,y2):xs)
makeLines (_:[]) = []

e1 = [('R',75),('D',30),('R',83),('U',83),('L',12),('D',49),('R',71),('U',7),('L',72)]
e2 = [('U',62),('R',66),('U',55),('R',34),('D',71),('R',55),('D',58),('R',83)]

x1 = [('R',98),('U',47),('R',26),('D',63),('R',33),('U',87),('L',62),('D',20),('R',33),('U',53),('R',51)]
x2 = [('U',98),('R',91),('D',20),('R',16),('D',67),('R',40),('U',7),('R',15),('U',6),('R',7)]
