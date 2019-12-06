import Data.List
import Data.Ord
import Data.List.Split
import qualified Data.MultiMap as MM

main = do
 x <- MM.fromList . map pairify . lines <$> readFile "6.txt"
 let i = filter (isUseful x) $ minimumBy (comparing length)
            $ filter (any (=="YOU")) $ filter (any (=="SAN"))
            [findAllChildren x e | e <- MM.keys x]
 print $ orbits x
 print $ length i

findAllChildren :: MM.MultiMap String String -> String -> [String]
findAllChildren m e = kids ++ (concatMap (findAllChildren m) kids)
  where kids = MM.lookup e m

isUseful :: MM.MultiMap String String -> String -> Bool
isUseful m e = any (== "YOU") kids || any (== "SAN") kids
  where kids = findAllChildren m e

orbits :: MM.MultiMap String String -> Int
orbits m = length . concat $ [findAllChildren m e | e <- MM.keys m]

pairify :: String -> (String, String)
pairify = (\(x:y:[]) -> (x,y)) . splitOn ")"
