import Data.List.Split
import Control.Lens
import Control.Monad.State

pair :: String -> (Char, String)
pair (x:xs) = (x,xs)

main = do
 (w1:w2:_) <-  (map.map) pair . map (splitOn ",") . lines <$> readFile "3.txt"
 print "yo wadup"



