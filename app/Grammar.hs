module Grammar where

import Data.Set    (Set)
import Data.Map    (Map, (!))
import Data.String (IsString(..))

data Term = Named String | Eof | Empty deriving (Eq, Ord)

data Point = Term Term | NonTerm String deriving (Eq, Ord, Show)

data Rule = MkRule
  { label  :: String
  , points :: [Point]
  } deriving (Eq, Ord, Show)

data Grammar = MkGrammar { rules :: Map String (Set Rule), start :: String } deriving Show

instance IsString Term where
  fromString "$eof" = Eof
  fromString "ε"    = Empty
  fromString other  = Named other

instance Show Term where
  show Eof       = "$eof"
  show Empty     = "ε"
  show (Named t) = t
