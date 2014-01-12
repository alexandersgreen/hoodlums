module Synacor where

import Prelude hiding (putChar,getChar,putStrLn,putStr)
import qualified Prelude as P

import Control.Applicative
import Control.Monad.State
import Data.Word
import Data.Bits
import qualified Data.Char as C
import qualified Data.IntMap as M

type Address = Int
type Offset = Address
type Value = Word16
type AddressMap = M.IntMap

data Machine = Machine
 { mem :: AddressMap Value
 , stack :: [Value]
 , ip :: Address
 , trace :: Bool 
 }

load :: [Word16] -> Machine
load prog | length prog <= 2^15 = Machine 
 { mem = M.fromList (zip [0..] prog)
 , stack = []
 , ip=0
 , trace = False
 }
load _ = error "program exceeds memory (machine has 15-bit address space)"

data Instruction = Halt | Set | Push | Pop | Eq | Gt | Jmp | Jt | Jf
  | Add | Mult | Mod | And | Or | Not | Rmem | Wmem | Call | Ret | Out
  | In | Noop 
 deriving Show

instruction :: Value -> Instruction
instruction val = case val of
  0  -> Halt
  1  -> Set
  2  -> Push
  3  -> Pop
  4  -> Eq
  5  -> Gt
  6  -> Jmp
  7  -> Jt
  8  -> Jf
  9  -> Add
  10 -> Mult
  11 -> Mod
  12 -> And
  13 -> Or
  14 -> Not
  15 -> Rmem
  16 -> Wmem
  17 -> Call
  18 -> Ret
  19 -> Out
  20 -> In
  21 -> Noop
  x  -> error $ "unknown instruction code: " ++ show x

args :: Instruction -> Int
args inst = case inst of
  Halt -> 0
  Set -> 2
  Push -> 1
  Pop -> 1
  Eq -> 3
  Gt -> 3
  Jmp -> 1
  Jt -> 2
  Jf -> 2
  Add -> 3
  Mult -> 3
  Mod -> 3
  And -> 3
  Or -> 3
  Not -> 2
  Rmem -> 2
  Wmem -> 2
  Call -> 1
  Ret -> 0
  Out -> 1
  In -> 1
  Noop -> 0

getValueFromOffset :: Offset -> Syn Value
getValueFromOffset offset = do
 ip <- gets ip
 mem <- gets mem
 let value = mem M.! (ip + offset)
 if value < 32768 
  then return value 
  else return $ maybe 0 id (M.lookup (fromIntegral value) mem)

getAddressFromOffset :: Offset -> Syn Address
getAddressFromOffset offset = do
 ip <- gets ip
 mem <- gets mem
 let addr = mem M.! (ip + offset)
 return $ fromIntegral addr    

putValueAt :: Address -> Value -> Syn ()
putValueAt address value = modify $ \s -> s {mem = M.insert address value (mem s)} 

getInstruction :: Syn Instruction
getInstruction = do
 value <- getValueFromOffset 0
 return $ instruction value

putChar :: Value -> Syn ()
putChar v = do
 let c = C.chr $ fromIntegral v
 debug <- gets trace
 if debug 
  then lift $ P.putStrLn $ show c
  else lift $ P.putChar c

putStrLn :: String -> Syn ()
putStrLn s = do
 debug <- gets trace
 if debug 
  then lift $ P.putStrLn s
  else return ()

putStr :: String -> Syn ()
putStr s = do
 debug <- gets trace
 if debug 
  then lift $ P.putStr s
  else return ()

debug :: Syn ()
debug = modify $ \s -> s{trace = True}

getChar :: Syn Word16
getChar = do
 c <- lift P.getChar
 return $ fromIntegral . C.ord $ c

type Syn = StateT Machine IO

run :: Machine -> IO ()
run m = evalStateT interpret m

interpret :: Syn ()
interpret = do
 s <- get
 instruction <- getInstruction
 putStr $ "ip=" ++ show (ip s) ++ ", stack=" ++ show (stack s) 
            ++ ", instruction=" ++ show instruction ++ " "
 case instruction of
  Halt -> return ()
  inst -> do 
   case inst of
    Out -> do
     arg <- getValueFromOffset 1
     putChar arg
     incip 2
     putStrLn ""
    Noop -> incip 1 >> putStrLn ""
    Jmp -> do
     arg <- getValueFromOffset 1
     setip arg
     putStrLn $ show arg
    Jt -> do
     arg1 <- getValueFromOffset 1
     putStr $ show arg1 ++ " "
     case arg1 of
      0 -> incip 3 >> putStrLn "_"
      _ -> do
       arg2 <- getValueFromOffset 2
       setip arg2
       putStrLn $ show arg2
    Jf -> do
     arg1 <- getValueFromOffset 1
     putStr $ show arg1 ++ " "
     case arg1 of
      0 -> do
       arg2 <- getValueFromOffset 2
       setip arg2
       putStrLn $ show arg2
      _ -> incip 3 >> putStrLn "_"
    Set -> do
     arg1 <- getAddressFromOffset 1
     arg2 <- getValueFromOffset 2
     putValueAt arg1 arg2
     incip 3
     putStrLn $ show arg1 ++ " " ++ show arg2
    Add -> do
     arg1 <- getAddressFromOffset 1
     arg2 <- getValueFromOffset 2
     arg3 <- getValueFromOffset 3
     putValueAt arg1 ((arg2 + arg3) `mod` 32768)
     incip 4
     putStrLn $ show arg1 ++ " " ++ show arg2 ++ " " ++ show arg3
    Eq -> do
     arg1 <- getAddressFromOffset 1
     arg2 <- getValueFromOffset 2
     arg3 <- getValueFromOffset 3
     putValueAt arg1 (if arg2 == arg3 then 1 else 0)
     incip 4
     putStrLn $ show arg1 ++ " " ++ show arg2 ++ " " ++ show arg3
    Push -> do
     arg1 <- getValueFromOffset 1
     modify $ \s -> s{stack = arg1:stack s}
     incip 2
     putStrLn $ show arg1 
    Pop -> do
     arg1 <- getAddressFromOffset 1
     st <- gets stack
     let (val,st') = case st of
                      [] -> error "empty stack"
                      (x:xs) -> (x,xs)
     putValueAt arg1 val
     modify $ \s -> s{stack = st'} 
     incip 2
     putStrLn $ show arg1
    Gt -> do
     arg1 <- getAddressFromOffset 1
     arg2 <- getValueFromOffset 2
     arg3 <- getValueFromOffset 3
     putValueAt arg1 (if arg2 > arg3 then 1 else 0)
     incip 4
     putStrLn $ show arg1 ++ " " ++ show arg2 ++ " " ++ show arg3
    And -> do
     arg1 <- getAddressFromOffset 1
     arg2 <- getValueFromOffset 2
     arg3 <- getValueFromOffset 3
     putValueAt arg1 (arg2 .&. arg3)
     incip 4
     putStrLn $ show arg1 ++ " " ++ show arg2 ++ " " ++ show arg3
    Or -> do
     arg1 <- getAddressFromOffset 1
     arg2 <- getValueFromOffset 2
     arg3 <- getValueFromOffset 3
     putValueAt arg1 (arg2 .|. arg3)
     incip 4
     putStrLn $ show arg1 ++ " " ++ show arg2 ++ " " ++ show arg3
    Not -> do
     arg1 <- getAddressFromOffset 1
     arg2 <- getValueFromOffset 2
     putValueAt arg1 (clearBit (complement arg2) 15)
     incip 3
     putStrLn $ show arg1 ++ " " ++ show arg2
    Call -> do
     arg <- getValueFromOffset 1
     addr <- gets ip
     modify $ \s -> s{stack = ((fromIntegral addr) + 2):stack s, ip = fromIntegral arg}
     s <- get
     putStrLn $ show arg
    Mult -> do
     arg1 <- getAddressFromOffset 1
     arg2 <- getValueFromOffset 2
     arg3 <- getValueFromOffset 3
     putValueAt arg1 ((arg2 * arg3) `mod` 32768)
     incip 4
     putStrLn $ show arg1 ++ " " ++ show arg2 ++ " " ++ show arg3
    Mod -> do
     arg1 <- getAddressFromOffset 1
     arg2 <- getValueFromOffset 2
     arg3 <- getValueFromOffset 3
     putValueAt arg1 (arg2 `rem` arg3)
     incip 4
     putStrLn $ show arg1 ++ " " ++ show arg2 ++ " " ++ show arg3
    Rmem -> do
     arg1 <- getAddressFromOffset 1
     arg2 <- getValueFromOffset 2
     mem <- gets mem
     let value = maybe 0 id (M.lookup (fromIntegral arg2) mem)
     putValueAt arg1 value
     incip 3
     putStrLn $ show arg1 ++ " " ++ show arg2 ++ " (" ++ show value ++ ")"
    x -> do
     debug <- gets trace
     modify $ \s -> s{trace = True}
     putStrLn $ "Unimplemented instruction: " ++ show x
     modify $ \s -> s{trace = debug}
     incip $ 1 + (args x)
   interpret

incip :: Int -> Syn ()
incip n = modify $ \s -> s{ip = ip s + n}

setip :: Word16 -> Syn ()
setip n = modify $ \s -> s{ip = fromIntegral n}

store :: Word16 -> Word16 -> Syn ()
store address value = do
 machine <- get
 put $ machine{mem = M.insert (fromIntegral address) value (mem machine)}

store' :: Word16 -> Word16 -> Syn ()
store' address value = modify $ \s -> s{mem = M.insert (fromIntegral address) value (mem s)}

getValue :: Int -> Syn Word16
getValue n = do
 i <- getValueFromOffset n
 if (i > 32767) then retrieve (fromIntegral i) else return i

retrieve :: Int -> Syn Word16
retrieve addr = (M.! addr) <$> gets mem 

