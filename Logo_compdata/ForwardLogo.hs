{-# LANGUAGE DeriveFunctor, TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-} 
{-# LANGUAGE UndecidableInstances, IncoherentInstances #-}

-- | This module contains the definition of our logo forward command, along
-- with an instance for evaluation, and parsing.
module ForwardLogo where

import SVG
import Eval
import Parse

import Data.Comp
import Data.Comp.Derive
import Data.Comp.Show

import Control.Monad.State

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))

-- | We can represent a distance as a double
type Distance = Double

-- | The data-type representing a forward command, automatically derives
-- an instance of the Functor type class. We also use compdata to derive
-- smart constructors.
data ForwardLogoF r = FD Distance
 deriving (Functor, Eq, Show)

derive [makeEqF, makeShowF,smartConstructors] [''ForwardLogoF]

-- | A forward command can be evaluated
instance Eval ForwardLogoF where
  evalAlg (FD d) = do
   (Turtle pos dir) <- get
   let pos' = offset pos dir d
   put (Turtle pos' dir)
   return [L pos']                    

-- | We calculate an /offset/ from the current position, heading
-- the given distance in the given direction
offset :: Position -> Direction -> Distance -> Position
offset (x,y) dir d = ( x + d * cos (dir / 180 * pi), 
                       y + d * sin (dir / 180 * pi) )

-- | A forward command can be generated by a parser
instance (ForwardLogoF :<: l, Functor l) => Parse ForwardLogoF l where
 parser _ = iFD <$> (string "fd" *> many1 space *> double)