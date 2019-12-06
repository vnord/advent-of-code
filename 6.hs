import Data.List
import Data.List.Split
import qualified Data.MultiMap as MM


main = do
 x <- MM.fromList . map pairify . lines <$> readFile "6.txt"
 print $ orbits x

parents = do
    x <- getMap
    pure $ MM.keys x

findAllChildren :: MM.MultiMap String String -> String -> [String]
findAllChildren m e = kids ++ (concatMap (findAllChildren m) kids)
  where kids = MM.lookup e m

orbits :: MM.MultiMap String String -> Int
orbits m = length . concat $ [findAllChildren m e | e <- MM.keys m]

getMap :: IO (MM.MultiMap String String)
getMap = MM.fromList . map pairify . lines <$> readFile "6-example.txt"
 
pairify :: String -> (String, String)
pairify = (\(x:y:[]) -> (x,y)) . splitOn ")"
