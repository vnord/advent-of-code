import Data.List.Split
import Control.Lens
import Control.Monad.State

main = do
 xs <- getArray <$> getInput
 let xs' = (head xs):12:2:(drop 3 xs)
 print $ compute xs'
 let correctProgram = (head . find) xs
 print correctProgram
 let (noun, verb) = (correctProgram!!1, correctProgram!!2)
 print $ 100*noun + verb

getInput :: IO String
getInput = init <$> readFile "2.txt"

getArray :: String -> [Int]
getArray = map read . splitOn ","

changeAt :: Int -> Int -> State [Int] ()
changeAt n v = modify $ element n .~ v 

add :: Int -> Int -> Int -> State [Int] ()
add p1 p2 d = do
 s <- get
 changeAt d (s!!p1 + s!!p2)

mul :: Int -> Int -> Int -> State [Int] ()
mul p1 p2 d = do
 s <- get
 changeAt d (s!!p1 * s!!p2)

run :: [Int] -> State [Int] [Int]
run (1:p1:p2:d:xs) = add p1 p2 d >> run xs
run (2:p1:p2:d:xs) = mul p1 p2 d >> run xs
run (99:_) = get

compute :: [Int] -> [Int]
compute xs = evalState (run xs) xs

find :: [Int] -> [[Int]]
find xs = filter (\x -> head x == 19690720)
  [compute ((head xs):n:v:(drop 3 xs)) | n <- [0..99], v <- [0..99]]
