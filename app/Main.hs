module Main where

import Grammar
import LR0 (getClosure, startRule)

import qualified Data.Set as Set
import qualified Data.List as List

listGrammar :: Grammar
listGrammar = Grammar
  { nonTerminals = Set.empty
  , terminals = Set.empty
  , declarated = Set.empty
  , rules = Set.fromList
      [ Rule "S'" [NonTerminal "S"]
      , Rule "S"  [Terminal "(", NonTerminal "L", Terminal ")"]
      , Rule "S"  [Terminal "x"]
      , Rule "L"  [NonTerminal "S"]
      , Rule "L"  [NonTerminal "L", Terminal ";", NonTerminal "S"]
      ]
  , start = "S'"
  }

main :: IO ()
main = do
  let (f, n) = (getFirstAndNullable listGrammar)
  let rules = (Set.toList listGrammar.rules)
  case List.find (\a -> a.name == listGrammar.start) rules of
    Just start ->  print (getClosure rules (Set.singleton (startRule start)))
    Nothing -> undefined