module LR0 where

import Grammar
import Data.Set (Set)
import Data.Map (Map, foldMapWithKey, (!))
import Data.List (intercalate)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.List as List

data LR0Item = LR0Item
  { position :: ([Term], Maybe Term, [Term])
  , rule :: Rule
  } deriving (Eq, Ord)

type State = Set LR0Item

instance Show LR0Item where
  show (LR0Item (before, t, after) (Rule label _)) =
    Text.unpack label <> " -> " <> intercalate " " (map show (reverse before) <> [".", maybe "" show t] <> map show after)

nextItem :: LR0Item -> Maybe LR0Item
nextItem (LR0Item (before, Nothing, _) rule) = Nothing
nextItem (LR0Item (before, Just locus, after) rule) = do
  (x, xs) <- List.uncons after
  pure $ LR0Item (locus : before, Just x, xs) rule

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
        Just (NonTerminal x) -> Set.fromList (map startRule (filter (\rule -> rule.name == x) rules))
        _ -> Set.empty

getGoto :: [Rule] -> Set LR0Item -> Term -> Set LR0Item
getGoto rules items term =
  getClosure
    rules
    (Set.fromList (catMaybes $ map nextItem $ filter (\item -> getLocus item == Just term) (Set.toList items)))

compile :: Grammar -> Rule -> (Map Int (Map Text (Action Int)), Int)
compile grammar start = do
    let lsRules = Set.toList grammar.rules
    let fstRule = getClosure lsRules (Set.singleton (startRule start))
    let startStates = Set.fromList [fstRule]
    let (states, transitions) = getTransitions startStates
    let indexed = Map.fromList (zip (Set.toList states) [0..])
    let (first, nullable) = getFirstAndNullable grammar
    let follow = getFollowSet grammar first nullable
    let table = foldl (foldMapper indexed transitions) (Map.empty) transitions
    let reductionSet = foldl (\reds state -> foldl (getReduction follow state) reds state) Set.empty states
    let table' = foldl (\table (state, term, prod) ->
                    let idx = (indexed ! state)
                        tableRes = monoidalLook idx table in
                    Map.insert idx (Map.insert term (Reduce prod) tableRes) table)
                    table reductionSet
    (table', indexed ! fstRule)
  where

    getReduction :: Follow -> Set LR0Item -> Set (State, Text, Rule) -> LR0Item -> Set (State, Text, Rule)
    getReduction follow state set item@(LR0Item (_, Nothing, _) rule) =
      case Map.lookup rule.name follow of
        Just fls -> set <> (Set.map (\s -> (state, s, rule)) fls)
        Nothing  -> set

    getReduction follow state set _ = set

    getTransitions :: Set State -> (Set State, Set (State, Term, State))
    getTransitions states = fixpoint (states, Set.empty :: Set (State, Term, State)) $
      \(states, trans) -> foldl (\t state -> foldl (\t i -> foldRes t state i) t state) (states, trans) states

    foldMapper :: Map State Int -> Set (State, Term, State) ->  Map Int (Map Text (Action Int)) -> (State, Term, State) ->  Map Int (Map Text (Action Int))
    foldMapper states transitions table (from, under, to) =
      let (fromS, toS) = (states ! from, states ! to) in
      case under of
        Terminal "$"  -> Map.insert fromS (Map.insert "$" (Accept toS) (monoidalLook fromS table)) table
        Terminal x    -> Map.insert fromS (Map.insert x (Shift toS) (monoidalLook fromS table)) table
        NonTerminal x -> Map.insert fromS (Map.insert x (Goto toS) (monoidalLook fromS table)) table

    foldRes :: (Set State, Set (State, Term, State)) -> State -> LR0Item -> (Set State, Set (State, Term, State))
    foldRes (states, trans) state item@(LR0Item (_, Nothing, _) rule) = (states, trans)
    foldRes (states, trans) state item@(LR0Item (_, Just term, _) rule) =
      let j = getGoto (Set.toList grammar.rules) state term in
      (Set.insert j states, Set.insert (state, term, j) trans)

data Action n = Shift n | Reduce Rule | Goto n | Accept n deriving Show

runGrammar :: Map Int (Map Text (Action Int)) -> Int -> [Text] -> [(Int, Text)] -> Either String ()
runGrammar table state [] stack       = error "Finished lol"
runGrammar table state (x : xs) stack =
  let res = table ! state in
  case Map.lookup x res of
    Just (Shift n)  -> runGrammar table n xs ((n, x) : stack)
    Just (Reduce r) ->
      case drop (length r.terms) stack of
        ((to, n) : st) -> runGrammar table to (r.name : x : xs) ((to, r.name) : (to, n) : st)
        _      -> runGrammar table 1 (r.name : x : xs) ((1, r.name) : [])
    Just (Goto n)   -> runGrammar table n (xs) stack
    Just (Accept n) -> pure ()
    Nothing -> error $ "Seila: " ++ show (x, state)