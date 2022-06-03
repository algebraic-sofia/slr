module Main where

import Grammar hiding (grammer)
import Parser

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map, (!))
import Data.Set (Set)

import Control.Monad.State
import Data.List (groupBy)
import Data.Function (on)
import Control.Arrow ((&&&))
import Data.Functor ((<&>))

-- Based on  Heimdell/LR

-- Example Grammar

test :: Grammar
test = MkGrammar
  (Map.fromList
    [ ("S'",
        Set.fromList
          [ MkRule "S'" [NonTerm "A"]
          , MkRule "S'" [Term "e"]])
    , ("B",
        Set.fromList
          [ MkRule "B" [Term "ε"]
          , MkRule "B" [Term "f"]])
    , ("A",
          Set.fromList [ MkRule "A" [NonTerm "B", Term "c"] ])
    ]) "Start"

-- Just a map that the inner elements join
newtype Moop k v = Moop (Map k v) deriving newtype (Eq)

instance (Show k, Show v) => Show (Moop k v) where
  show (Moop a) = show a

instance (Ord k, Monoid v) => Semigroup (Moop k v) where
  Moop a <> Moop b = Moop (Map.unionWith (<>) a b)

instance (Ord k, Monoid v) => Monoid (Moop k v) where
  mempty = Moop mempty

singleton :: k -> v -> Moop k v
singleton k v = Moop (Map.singleton k v)

-- Now it's about compiling using FIRST, FOLLOW, GOTO and Action

type FirstTable = Moop String (Set Term)

(?!) :: (Monoid b, Ord k) => Moop k b -> k -> b
(?!) (Moop m) k = maybe mempty id (Map.lookup k m)

(?!~) :: (Monoid b, Ord k) => Map k b -> k -> b
(?!~) m k = maybe mempty id (Map.lookup k m)

fixpoint :: Eq a => a -> (a -> a) -> a
fixpoint state fn =
  let res = fn state in
  if res == state
    then res
    else fixpoint res fn

getFirstTable :: Grammar -> FirstTable
getFirstTable (MkGrammar rules start) =
    fixpoint (singleton start Set.empty)
      $ \firsts -> (foldMap . foldMap) (foldRule firsts) rules
  where
    foldRule lastFirst (MkRule label [])       =
      singleton label (Set.singleton Empty)

    foldRule lastFirst (MkRule label (x : xs)) =
      case x of
        Term Empty -> foldRule lastFirst (MkRule label xs) <> singleton label (Set.singleton Empty)
        Term x     -> singleton label (Set.singleton x)
        NonTerm x  ->
          if Set.member Empty (lastFirst ?! x)
            then singleton label (lastFirst ?! x) <> foldRule lastFirst (MkRule label xs)
            else singleton label (lastFirst ?! x)

-- Now the follow

type GotoTable = Map Int (Map Point Int)

data Item = Item
  { entity     :: String
  , pos        :: ([Point], Maybe Point, [Point])
  , look       :: Set Term
  , itemLabel  :: String
  } deriving (Eq, Ord)

data PState = PState { idx :: Int, items :: Set Item, kernel :: Set Item }
  deriving (Eq, Ord)

data Reg = Reg
  { states  :: Map PState Int
  , indices :: Map Int PState
  , firstT  :: FirstTable
  , counter :: Int
  }

emptyReg :: FirstTable -> Reg
emptyReg f = Reg Map.empty Map.empty f 0

locus :: Item -> Maybe Point
locus (Item _ (_, f, _) _ _) = f

next :: Item -> Maybe Item
next item =
    nextPos item.pos <&> \pos -> item { pos }
  where
    nextPos = \case
      (b, Just l, n : r) -> Just ((l:b, Just n, r))
      (b, Just l, [])    -> Just ((l:b, Nothing, []))
      _                  -> Nothing

ruleStart :: Set Term -> Rule -> Item
ruleStart lookahead rule@(MkRule label (x : xs)) =
  Item label ([], Just x, xs) lookahead label

closure :: MonadState Reg m => Grammar -> FirstTable -> Set Item -> m (Int, Bool)
closure grammar first items =
    let state = fixpoint items goesTo in
    register (PState (error "Not set") (normalize state) items)
  where

    goesTo :: Set Item -> Set Item
    goesTo = foldMap $ \item ->
      case locus item of
        Nothing          -> mempty
        Just (Term x)    -> mempty
        Just (NonTerm x) ->
          let ahead = nextLook item in
          Set.map (ruleStart ahead) (grammar.rules ?!~ x)

    nextLook item =
      case next item >>= locus of
        Nothing             -> look item
        Just (Term term)    -> Set.singleton term
        Just (NonTerm term) -> first ?! term

    normalize :: Set Item -> Set Item
    normalize = Set.fromList
              . fmap (\list@(x:_) -> x { look = Set.unions $ fmap look list })
              . groupBy ((==) `on` (entity &&& itemLabel &&& pos))
              . Set.toList

    createReg :: MonadState Reg m => PState -> m (Int, Bool)
    createReg state = do
      idx <- gets counter
      let state' = state { idx }
      modify $ \(Reg states indices first counter) ->
        Reg (Map.insert state' idx states)
            (Map.insert idx state' indices)
            first
            (counter + 1)
      pure (idx, True)

    register :: MonadState Reg m => PState -> m (Int, Bool)
    register state = do
      state' <- gets (Map.lookup state . states)
      maybe (createReg state) (pure . (, False)) state'

firstState :: MonadState Reg m => Grammar -> m Int
firstState grammar = do
    fst' <- gets firstT
    fst <$> closure grammar fst' firstItem
  where
    firstRule = head $ Set.toList $ (grammar.rules ! grammar.start)
    firstItem = Set.singleton (ruleStart (Set.singleton Eof) firstRule)

getGotoTable :: MonadState Reg m => Grammar -> m GotoTable
getGotoTable grammar = do
    fstState <- firstState grammar
    loop (Set.singleton fstState) Map.empty
  where
    loop pool goto = undefined


main :: IO ()
main = do
  res <- readFile "ata.bnk"
  case parseGrammar res of
    Left err -> putStrLn err
    Right res -> print $ getFirstTable res