module Grammar where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map, (!))
import Data.Set (Set)

import Control.Monad.State ( MonadState, gets, modify, foldM )
import Data.List (groupBy, intercalate, sortBy)
import Data.Function (on)
import Control.Arrow ((&&&))
import Data.Functor ((<&>))

import Data.Set    (Set)
import Data.Map    (Map, (!))
import Data.String (IsString(..))
import Data.Maybe  ( mapMaybe )

-- Based on  Heimdell/LR

data Term = Named String | Eof | Empty deriving (Eq, Ord)

data Point = Term Term | NonTerm String deriving (Eq, Ord)

data Rule = MkRule
  { label  :: String
  , points :: [Point]
  } deriving (Eq, Ord, Show)

data Grammar = MkGrammar { rules :: Map String (Set Rule), start :: String } deriving Show

instance Show Term where
  show Eof       = "$"
  show Empty     = "ε"
  show (Named t) = t

-- Just a map that the inner elements join

newtype Moop k v = Moop (Map k v) deriving newtype (Eq)

instance (Show k, Show v) => Show (Moop k v) where show (Moop a) = show $ Map.toList a
instance (Ord k, Monoid v) => Semigroup (Moop k v) where Moop a <> Moop b = Moop (Map.unionWith (<>) a b)
instance (Ord k, Monoid v) => Monoid (Moop k v) where mempty = Moop mempty

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
  } deriving (Eq, Ord)


instance Show Item where
  show (Item entity (bef, Just m, later) look)  =
    "{ " ++ (show $ Term $ Named entity) ++ " -> " ++ intercalate " " (map show bef) ++ "• " ++ show m <> " " <> intercalate " " (map show later)  ++ " | " ++ intercalate ", " (map show (Set.toList look)) ++ " }"

  show (Item entity (bef, Nothing, later) look) =
    "{ " ++ (show $ Term $ Named entity) ++ " -> " ++ intercalate " " (map show bef) ++ "•" ++ " | " ++ intercalate ", " (map show (Set.toList look)) ++ " }"

instance Show Point where
  show (Term term)    = "\x1b[34m" <> show term <> "\x1b[0m"
  show (NonTerm term) = "\x1b[32m" <> term <> "\x1b[0m"

instance Show PState where
  show (PState idx items kernel) =
    "{ " <> intercalate ", " [show idx, show $ Set.toList items, show $ Set.toList kernel] <> " }"

instance Show Reg where
  show (Reg states indices _ c) =
      intercalate ",\n" $ map arrow (Map.toList indices)
    where
      arrow (a, b) = show a <> " => " <> show b

data PState = PState { idx :: Int, items :: Set Item, kernel :: Set Item }

instance Eq PState where (==) = (==) `on` items
instance Ord PState where compare = (compare) `on` items

data Reg = Reg
  { states  :: Map PState Int
  , indices :: Map Int PState
  , firstT  :: FirstTable
  , counter :: Int
  }

emptyReg :: FirstTable -> Reg
emptyReg f = Reg Map.empty Map.empty f 0

locus :: Item -> Maybe Point
locus (Item _ (_, f, _) _) = f

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
  Item label ([], Just x, xs) lookahead

closure :: MonadState Reg m => Grammar -> FirstTable -> Set Item -> m (Int, Bool)
closure grammar first items = do
    let state = fixpoint (items) (\s -> goesTo s <> s) <> items
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
              . groupBy ((==) `on` (entity &&& pos))
              . Set.toList

    createReg :: MonadState Reg m => PState -> m (Int, Bool)
    createReg oldState = do
      idx <- gets counter
      let state' = oldState { idx }
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

    loop :: MonadState Reg m => Set Int -> GotoTable -> m GotoTable
    loop pool goto = do
      (pool', goto') <- foldM add (Set.empty, goto) pool
      if null pool' then pure goto' else loop pool' goto'

    add :: MonadState Reg m => (Set Int, GotoTable) -> Int -> m (Set Int, GotoTable)
    add (pool, goto) idx = do
      let goto' | Map.member idx goto = goto
                | otherwise = Map.insert idx Map.empty goto

      mater <- gets ((! idx) . indices)

      let itemsByNextPoint =
              fmap (\pairs@((p, _) : _) -> (p, Set.fromList (map snd pairs)))
            . groupBy ((==) `on` fst)
            . sortBy (compare `on` fst)
            . mapMaybe (\r -> (,) <$> locus r <*> next r)
            . Set.toList
            . items
            $ mater

      foldM (addItem idx) (pool, goto') itemsByNextPoint

    addItem :: MonadState Reg m => Int -> (Set Int, GotoTable) -> (Point, Set Item) -> m (Set Int, GotoTable)
    addItem idx (pool, goto) (point, items) = do
      first <- gets firstT
      (nextIdx, new) <- closure grammar first items
      let pool' = if new then Set.insert nextIdx pool else pool
      let goto' = Map.adjust (Map.insert point nextIdx) idx goto
      pure (pool', goto')