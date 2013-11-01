{-# LANGUAGE TypeOperators, OverlappingInstances, ScopedTypeVariables #-}

-- | This Main module composes the three currently defined evaluable and
-- parseable types, giving us our current logo language. The main function
-- expects two arguments, the first is an input file that is parsed, the second
-- is an output file that the generated svg is written to.
module Main where

import SVG
import Eval
import Parse

import ForwardLogo
import RightLogo
import RepeatLogo

import Data.Comp
import Data.Comp.Ops

import System.Environment

-- | Currently our logo language consists of forward commands, right 
-- turn commands, and repetition commands.
type LogoF =  ForwardLogoF :+: RightLogoF :+: RepeatLogoF
type Logo = Term LogoF

-- | An input string can be parsed to generate SVG. Note that we must
-- explicitly type our parsed terms, and give an explicityl type dummy
-- term to the parser.
logoSVG :: String -> String -> SVG
logoSVG source input = eval prog
 where
  prog :: [Logo] = (parseTerms source input (dummy :: Logo))

-- | The main function expects two arguments, the first is an input file 
-- that is parsed, the second is an output file that the generated svg is 
-- written to.
main :: IO ()
main = do
 argv <- getArgs
 case length argv == 2 of
  False -> do
   putStrLn "usage: logo input.logo output.svg"
  True -> do
   let inputFile = argv !! 0
   let outputFile = argv !! 1
   logo <- readFile inputFile
   let svg = logoSVG inputFile logo 
   writeFile outputFile $ show svg
 
