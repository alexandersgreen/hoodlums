{-# language TupleSections #-}

module Huffman where

import Data.Map (Map, empty, fromListWith, toList, singleton, union, (!))
import qualified Data.Map as Map
import Data.List (sortBy,insertBy)
import Data.Ord (comparing)

frequencies :: String -> Map Char Int
frequencies = fromListWith (+) . map (,1)

data Tree = Leaf Char Int | Node Int Tree Tree deriving Show

size :: Tree -> Int
size (Leaf _ s)   = s
size (Node s _ _) = s

mkLeaf :: (Char,Int) -> Tree
mkLeaf (c,i) = Leaf c i

mkNode :: Tree -> Tree -> Tree
mkNode t1 t2 = Node (size t1 + size t2) t1 t2

mkTree :: Map Char Int -> Tree
mkTree = build . map mkLeaf . sortBy (comparing snd) . toList

insert :: Tree -> [Tree] -> [Tree]
insert = insertBy (comparing size)

build :: [Tree] -> Tree
build [] = error "No text"
build [t] = t
build (t1 : t2 : ts) = build $ insert (mkNode t1 t2) ts

type BitString = [Bool]
type Table = Map Char BitString

traverse :: Tree -> Table
traverse (Leaf c _) = singleton c []
traverse (Node _ t1 t2) = union p1' p2'
 where
  p1 = traverse t1
  p1' = Map.map ((:) False) p1
  p2 = traverse t2 
  p2' = Map.map ((:) True) p2

mkBitStrings :: Tree -> Table
mkBitStrings = mkBitStrings' []
 where
  mkBitStrings' p (Leaf c _) = singleton c (reverse p)
  mkBitStrings' p (Node _ l r) = 
   mkBitStrings' (True : p) l `union` mkBitStrings' (False : p) r

mkTable :: String -> Table
mkTable = mkBitStrings . mkTree . frequencies

encodeWith :: Table -> String -> BitString
encodeWith tbl = concatMap (tbl !)

encode :: String -> (Tree,BitString)
encode s = (tree,encodeWith tbl s)
 where
  tree = mkTree . frequencies $ s
  tbl = mkBitStrings tree

decode :: Tree -> BitString -> String
decode tree = decode' tree
 where  
  decode' (Leaf c _)   bs           = c : decode tree bs
  decode' _ [] = []
  decode' (Node _ l _) (True : bs)  = decode' l bs
  decode' (Node _ _ r) (False : bs) = decode' r bs

test :: String -> (String,Int,Int)
test s = (decode tree enc,length s,(length enc `div` 8))
 where
  (tree,enc) = encode s  

main :: IO ()
main = do
  source <- readFile "sample.txt"
  let 
   res = test source
  print res

