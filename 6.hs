import Data.List
import Data.List.Split
import qualified Data.MultiMap as MM
import Data.Tree.Class
import Data.Tree.NTree.TypeDefs


main = do
 x <- MM.fromList . map pairify . lines <$> readFile "6-example.txt"
 print "6"

getMap :: IO (MM.MultiMap String String)
getMap = MM.fromList . map pairify . lines <$> readFile "6-example.txt"
 
pairify :: String -> (String, String)
pairify = (\(x:y:[]) -> (x,y)) . splitOn ")"
