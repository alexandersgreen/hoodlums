{-# LANGUAGE TypeOperators #-}

-- | This module contains the type class definition for functors that can
-- be evaluated. In this context, evaluation defines an Algebra that outputs a
-- stateful computation that produces a list of steps, with a /Turtle/ as the 
-- state. This module also provides the instance for evaluation of the composition 
-- of two evaluable sub-data-types, and the overall evaluation function defined
-- as the catamorphism of the evaluation Algebra. 
module Eval where

import SVG
import Data.Comp
import Data.Comp.Ops
import Control.Monad.State

-- | A /Direction/ is simply an angle, representing the current heading.
type Direction = Double

-- | For now, a /Turtle/ stores its current position, and heading.
data Turtle = Turtle Position Direction

-- | The type-class definition for functors that can be evaluated.
class (Functor f) => Eval f where
  evalAlg :: Alg f (State Turtle [Step])

-- |  The instance for evaluation of the composition of two evaluable 
-- sub-data-types
instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlg (Inl x) = evalAlg x
  evalAlg (Inr x) = evalAlg x

-- | The evaluation function defined as the catamorphism of the evaluation
-- Algebra produces a stateful computation
evalM :: (Eval f) => Term f -> State Turtle [Step]
evalM = cata evalAlg

-- | The overall evaluation function also evaluates the stateful computation
-- starting with an initial Turtle.
eval :: (Eval f) => [Term f] -> SVG
eval ts = Path $ concat $ evalState (sequence $ map evalM ts) (Turtle (200,200) 0)



