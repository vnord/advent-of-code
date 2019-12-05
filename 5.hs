{-# LANGUAGE TemplateHaskell #-}
import Data.List.Split
import Control.Lens
import Control.Monad.RWS.Lazy
import Control.Monad.State
import System.IO.Unsafe
import Debug.Trace

type Program = RWS [Int] [Int] St

data St = St {
  _prog :: [String],
  _inputCounter :: Int,
  _programCounter :: Int
}


makeLenses ''St

main = do
 xs <- getArray <$> getInput
 print $ evalRWS run [1] (St xs 0 0)
 print $ evalRWS run [5] (St xs 0 0)

getInput :: IO String
getInput = init <$> readFile "5.txt"

getArray :: String -> [String]
getArray = splitOn "," 

changeAt :: Int -> Int -> Program ()
changeAt n v = do
  p <- use prog
  let (x,_:ys) = splitAt n p
  prog .= x ++ (show v : ys)
  return ()

add :: Int -> Int -> Int -> Program ()
add v1 v2 d = changeAt d (v1 + v2)

mul :: Int -> Int -> Int -> Program ()
mul v1 v2 d = changeAt d (v1 * v2)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

(!?) :: [a] -> Int -> a
(!?) = (!!) . reverse


run :: Program ()
run = do
  pc <- use programCounter
  prog <- use prog
  let  instr = prog !! pc
  let instr' = case instr of "1"   -> "0001"
                             "101" -> "0101"
                             "2"   -> "0002"
                             "102" -> "0102"
                             "3"   -> "03"
                             "4"   -> "004"
                             "7"   -> "0007"
                             "8"   -> "0008"
                             _   -> instr
  let  opcode = lastN 2 instr'
  let  m1 = if (length instr > 2) then instr !? 2 else '0'
  let  m2 = if (length instr > 3) then instr !? 3 else '0'
  -- let  m2 = instr' !? 3
  let  p1 = read $ prog !! (pc+1)
  let  p2 = read $ prog !! (pc+2)
  let  p3 = read $ prog !! (pc+3)
                            
  case opcode of 
    -- "01" -> add <$> getVal p1 m1 <*> getVal p2 m2 <*> pure p3 >> programCounter += 4 >> run
    -- "02" -> mul <$> getVal p1 m1 <*> getVal p2 m2 <*> pure p3 >> programCounter += 4 >> run
    "01" -> do
        v1 <- getVal p1 m1
        v2 <- getVal p2 m2
        add v1 v2 p3
        programCounter += 4
        run
    "02" -> do
        v1 <- getVal p1 m1
        v2 <- getVal p2 m2
        mul v1 v2 p3
        programCounter += 4
        run
    "03" -> do
        input <- ask
        ipc   <- use inputCounter
        changeAt p1 (input !! ipc)
        inputCounter += 1
        programCounter += 2
        x <- gets _prog
        run
    "04" -> do
        v <- getVal p1 m1
        tell [v]
        programCounter += 2
        run
    "05" -> do
        v1 <- getVal p1 m1
        v2 <- getVal p2 m2
        if v1 /= 0 then programCounter .= v2 else programCounter += 3
        run
    "06" -> do
        v1 <- getVal p1 m1
        v2 <- getVal p2 m2
        if v1 == 0 then programCounter .= v2 else programCounter += 3
        run
    "07" -> do
        v1 <- getVal p1 m1
        v2 <- getVal p2 m2
        if v1 < v2 then changeAt p3 1 else changeAt p3 0
        programCounter += 4
        run
    "08" -> do
        v1 <- getVal p1 m1
        v2 <- getVal p2 m2
        if v1 == v2 then changeAt p3 1 else changeAt p3 0
        programCounter += 4
        run
    "99" -> return ()
    _ -> error $ "pc: " <> show pc <> ", opcode: " <> show opcode

getVal :: Int -> Char -> Program Int
getVal i '0' = do
  s <- use prog
  pure . read $ s!!i
getVal i '1' = return i
