module Main where

import Grammar
import LR0

import qualified Data.Set as Set
import qualified Data.List as List

import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.List

listGrammar :: Grammar
listGrammar = Grammar
  { nonTerminals = Set.empty
  , terminals = Set.empty
  , declarated = Set.empty
  , rules = Set.fromList
      [ Rule "S'" [NonTerminal "S", Terminal "$"]
      , Rule "S"  [Terminal "(", NonTerminal "L", Terminal ")"]
      , Rule "S"  [Terminal "x"]
      , Rule "L"  [NonTerminal "S"]
      , Rule "L"  [NonTerminal "L", Terminal ";", NonTerminal "S"]
      ]
  , start = "S'"
  }


main :: IO ()
main = do
  let rules = (Set.toList listGrammar.rules)
  let t = "(x;x)$"
  case List.find (\a -> a.name == listGrammar.start) rules of
    Just start -> do
      let (table, fst') = compile listGrammar start
      putStrLn $ intercalate "\n" (map show $ Map.toList table)
      print $ runGrammar table fst' (map Text.singleton t) []
    Nothing -> undefined