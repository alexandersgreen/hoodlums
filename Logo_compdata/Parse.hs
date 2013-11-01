{-# LANGUAGE TypeOperators, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

-- | This module contains the type class definition for functors that can
-- be parsed. In this context, parsing defines an Algebra that outputs a
-- Parsec parser for any super-type of the type being parsed. In order to
-- achieve this, dummy terms are passed round, which are manually typed to
-- give the type-checker enough information. This module also provides the 
-- instance for parsing the composition of two parseble sub-data-types, which
-- introduces back-tracking into the parser. 
module Parse where

import SVG
import Data.Comp
import Data.Comp.Ops

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))

import Debug.Trace

-- | The type class definition for functors that can be parsed, into a term
-- of a given super-type.
class (Functor f, Functor g) => Parse f g where
  parser :: Alg f (Parser (Term g))

-- | A dummy term, that can take any type. An error will be thrown if this term
-- is evaluated, so these terms should only be used to give information to the
-- type-checker.
dummy :: a
dummy = error "evaluation of dummy term"

-- | The instance for parsing the composition of two parseble sub-data-types.
-- Note that this introduces back-tracking into the parser, with the use of /try/. 
instance (Parse f h, Parse g h) => Parse (f :+: g) h where
  parser _ = do
   let parser_f :: Parser (Term h) = parseTerm (dummy :: Term f)
   let parser_g :: Parser (Term h) = parseTerm (dummy :: Term g)
   try parser_f <|> parser_g

-- | Parsing a term is achieved using the catamorphism of the parsing Algebra.
-- Note that the input argument to this function isn't evaluated, and is required
-- for typing purposes, enabling a dummy term to be used.
parseTerm :: (Parse f g) => Term f -> Parser (Term g)
parseTerm = cata parser

-- | The logo language consists of multiple terms that can be seperated by
-- whitespace. Again, the input argument to this function isn't evaluated, 
-- and is required for typing purposes, enabling a dummy term to be used.
terms :: (Parse f g) => Term f -> Parser [Term g]
terms x = (parseTerm x) `sepBy` (many1 space)

-- | The /terms/ parser can be used to parse a string containing a fragment
-- of logo. Again, the third input argument to this function isn't evaluated, 
-- and is required for typing purposes, enabling a dummy term to be used.
parseTerms :: (Parse f g) => String -> String -> Term f -> [Term g]
parseTerms source input x = case parse (terms x) source input of
 Left err -> trace (show err) []
 Right ts -> ts

-- | A simple parser for a Double. Note that this can currently fail in
-- rather horrible ways, e.g. it will parse "4.5.6.7", but the call to 
-- /read/ will fail.
double :: Parser Double
double = read <$> (many1 (digit <|> char '.'))

-- | A simple parser for an Integer.
integer :: Parser Integer
integer = read <$> many1 digit