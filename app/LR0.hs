module LR0 where

import Data.Set (Set)
import Data.Map (Map)

import qualified Data.Set as Set
import qualified Data.Map as Map

import Grammar
import qualified Data.Text as Text
import Data.List (intercalate)
import Debug.Trace (traceShowId, trace)

data LR0Item = LR0Item
  { position :: ([Term], Maybe Term, [Term])
  , rule :: Rule
  } deriving (Eq, Ord)

instance Show LR0Item where
  show (LR0Item (before, Just t, after) (Rule label _)) =
    Text.unpack label <> " -> " <> intercalate " " (map show before <> [".", show t] <> map show after)
  show (LR0Item (before, Nothing, after) (Rule label _)) =
    Text.unpack label <> " -> " <> intercalate " " (map show before <> ["."])

nextItem :: LR0Item -> Maybe LR0Item
nextItem (LR0Item (before, Just locus, (x : xs)) rule) = Just $ LR0Item (locus : before, Just x, xs) rule
nextItem (LR0Item (before, Just locus, []) rule) = Just $ LR0Item (locus : before, Nothing, []) rule
nextItem (LR0Item (before, Nothing, _) rule) = Nothing

getLocus :: LR0Item -> Maybe Term
getLocus (LR0Item (_, m, _) _ ) = m

startRule :: Rule -> LR0Item
startRule rule@(Rule _ (x : xs)) = LR0Item ([], Just x,  xs) rule
startRule rule@(Rule _ [])       = LR0Item ([], Nothing, []) rule

getClosure :: [Rule] -> Set LR0Item -> Set LR0Item
getClosure rules set =
    fixpoint set (\r -> foldMap mapItem r <> r)
  where
    mapItem :: LR0Item -> Set LR0Item
    mapItem item =
      case getLocus item of
        Just (NonTerminal x) ->
          Set.fromList (map startRule (filter (\rule -> rule.name == x) rules))
        _ -> Set.empty

getGoto :: [Rule] -> Set LR0Item -> Term -> Set LR0Item
getGoto rules items term =
  getClosure rules (Set.fromList (filter (\item -> getLocus item == Just term) (Set.toList items)))