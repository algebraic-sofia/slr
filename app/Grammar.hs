module Grammar where

import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import Data.List (intercalate)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text
import Debug.Trace (trace)

data Term = Terminal { text :: Text} | NonTerminal { text :: Text} deriving (Eq, Ord)

data Rule = Rule
    { name :: Text
    , terms :: [Term]
    } deriving (Eq, Ord)

data Grammar = Grammar
  { nonTerminals :: Set Term
  , terminals    :: Set Term
  , declarated   :: Set Term
  , rules        :: Set Rule
  , start        :: Text
  } deriving Show

instance Show Term where
  show (Terminal text)    = Text.unpack text
  show (NonTerminal text) = Text.unpack text

instance Show Rule where
  show (Rule label terms) = Text.unpack label <> " -> " <> intercalate " " (map show terms)

type Follow = Map Text (Set Text)
type First = Map Text (Set Text)

fixpoint :: Eq a => a -> (a -> a) -> a
fixpoint state fn =
  let res = fn state in
  if res == state
    then res
    else fixpoint res fn

monoidalLook :: (Monoid v, Ord k) => k -> Map k v -> v
monoidalLook k m = maybe mempty id (Map.lookup k m)

insertAdd :: (Ord a, Ord b) => a -> Set b -> Map a (Set b) -> Map a (Set b)
insertAdd k v map =
  case Map.lookup k map of
    Just res -> Map.insert k (res <> v) map
    Nothing  -> Map.insert k v map

getFirstAndNullable :: Grammar -> (First, Set Text)
getFirstAndNullable grammar = do
    fixpoint (Map.singleton grammar.start Set.empty, Set.empty) $
      \state -> foldr (flip $ uncurry getSet) state grammar.rules
  where
    getSet :: First -> Set Text -> Rule -> (First, Set Text)
    getSet first nullable (Rule label [])       = (first, Set.insert label nullable)
    getSet first nullable (Rule label (x : xs)) =
      case x of
        Terminal    x -> (insertAdd label (Set.singleton x) first, nullable)
        NonTerminal x ->
          case (Set.member x nullable, Map.lookup x first) of
            (True, _)         -> getSet first nullable (Rule label xs)
            (False, Just res) -> (insertAdd label res first, nullable)
            (False, Nothing)  -> (first, nullable)

getFollowSet :: Grammar -> Follow -> Set Text -> Follow
getFollowSet grammar firstSet nullable = do
    fixpoint (Map.singleton grammar.start Set.empty) $
      \state -> foldr (flip (getSet)) state grammar.rules
  where
    accAfter :: Text -> Text -> Follow -> [Term] -> Follow
    accAfter first label follow []       = insertAdd first (monoidalLook label follow) follow
    accAfter first label follow (x : xs) =
      case x of
        Terminal x -> insertAdd first (Set.singleton x) follow
        NonTerminal x
          | Set.member x nullable -> accAfter first label follow xs
          | otherwise -> insertAdd first (monoidalLook x firstSet) follow

    getSet :: Follow -> Rule -> Follow
    getSet follow (Rule label []) = follow
    getSet follow (Rule label (Terminal x : xs)) = getSet follow (Rule label xs)
    getSet follow (Rule label (NonTerminal x : xs)) = getSet (accAfter x label follow xs) (Rule label xs)
