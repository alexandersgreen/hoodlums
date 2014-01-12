module Main where

import Synacor hiding (putStrLn)

import Data.Word
import qualified Data.ByteString as B
import Data.Bits

main :: IO ()
main = do
 machine <- loadMachine
 run machine

lookupCode :: Int -> IO ()
lookupCode addr = do
 bs <- B.readFile "challenge.bin"
 let prog = B.unpack bs
 let instlo = prog !! (2 * addr)
 let insthi = prog !! ((2 * addr) + 1)
 let inst = head $ combine [instlo,insthi]
 putStrLn $ "(" ++ show instlo ++ "," ++ show insthi ++ ") -> " ++ show inst
            ++ if (inst <= 21) then (" (" ++ show (instruction inst) ++ ")") else ""

loadMachine :: IO Machine
loadMachine = do
 bs <- B.readFile "challenge.bin"
 let prog = B.unpack bs
 let machine = load $ combine prog
 return machine

combine :: [Word8] -> [Word16]
combine [] = []
combine [l] = [fromIntegral l]
combine (l:h:rest) = w16:combine rest
 where
  w16 = fromIntegral l + (shiftL (fromIntegral h) 8)
