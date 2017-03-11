{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Rose where

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..), liftM, ap, mfilter, join, replicateM)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Control.Monad.Trans.Writer.Lazy (WriterT(..), tell)

import           Data.Bifunctor (bimap, second)
import           Data.Foldable (for_)
import           Data.Maybe (mapMaybe, fromJust, isJust)

import           Prelude hiding (filter)

import qualified System.Random as Random

------------------------------------------------------------------------
-- Seed

newtype Seed =
  Seed Random.StdGen

mkSeed :: Int -> Seed
mkSeed =
  Seed . Random.mkStdGen

newSeed :: IO Seed
newSeed =
  fmap Seed Random.newStdGen

splitSeed :: Seed -> (Seed, Seed)
splitSeed (Seed s) =
  bimap Seed Seed $ Random.split s

nextInteger :: Integer -> Integer -> Seed -> (Integer, Seed)
nextInteger lo hi (Seed s) =
  second Seed $ Random.randomR (lo, hi) s

------------------------------------------------------------------------
-- Tree

data Tree a =
  Node a [Tree a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

outcome :: Tree a -> a
outcome (Node x _) =
  x

shrinks :: Tree a -> [Tree a]
shrinks (Node _ xs) =
  xs

instance Monad Tree where
  return x =
    Node x []

  (>>=) m k =
    let
      Node x xs =
        m
      Node y ys =
        k x
    in
      Node y $
        fmap (>>= k) xs ++ ys

instance Applicative Tree where
  pure =
    return
  (<*>) =
    ap

unfoldTree :: (b -> a) -> (b -> [b]) -> b -> Tree a
unfoldTree f g x =
  Node (f x) (unfoldForest f g x)

unfoldForest :: (b -> a) -> (b -> [b]) -> b -> [Tree a]
unfoldForest f g =
  fmap (unfoldTree f g) . g

expandTree :: (a -> [a]) -> Tree a -> Tree a
expandTree f (Node x xs) =
  Node x $
    fmap (expandTree f) xs ++ unfoldForest id f x

pruneTree :: Tree a -> Tree a
pruneTree (Node x _) =
  Node x []

mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f (Node x xs) = do
  y <- f x
  pure . Node y $
    mapMaybe (mapMaybeTree f) xs

------------------------------------------------------------------------
-- Gen

newtype Gen a =
  Gen (Seed -> Maybe (Tree a))

runGen :: Seed -> Gen a -> Maybe (Tree a)
runGen seed (Gen g) =
  g seed

mapGen :: (Maybe (Tree a) -> Maybe (Tree b)) -> Gen a -> Gen b
mapGen f (Gen m) =
  Gen (\s -> f (m s))

sampleGen :: Gen a -> IO (Maybe (Tree a))
sampleGen gen = do
  seed <- newSeed
  pure (runGen seed gen)

instance Monad Gen where
  return =
    Gen . const . return . return

  (>>=) m k =
    Gen $ \s ->
      case splitSeed s of
        (sk, sm) -> do
          fmap join $
            mapMaybeTree (runGen sk . k) =<< runGen sm m

instance MonadPlus Gen where
  mzero =
    Gen $ const Nothing

  mplus x y =
    Gen $ \s ->
      case splitSeed s of
        (sx, sy) ->
          runGen sx x `mplus`
          runGen sy y

instance Functor Gen where
  fmap =
    liftM

instance Applicative Gen where
  pure =
    return
  (<*>) =
    ap

instance Alternative Gen where
  empty =
    mzero
  (<|>) =
    mplus

------------------------------------------------------------------------
-- Shrinking

towards :: Integral a => a -> a -> [a]
towards destination x =
  if destination == x then
    []
  else
    let
      -- We need to halve our operands before subtracting them as they may be
      -- using the full range of the type (i.e. 'minBound' and 'maxBound' for
      -- 'Int32')
      diff =
        (x `quot` 2) - (destination `quot` 2)
    in
      -- We make up for halving the inputs by explicitly prepending the
      -- destination as the first element of the list.
      destination `consNub` fmap (x -) (halves diff)

consNub :: Eq a => a -> [a] -> [a]
consNub x ys0 =
  case ys0 of
    [] ->
      x : []
    y : ys ->
      if x == y then
        y : ys
      else
        x : y : ys

shrinkList :: [a] -> [[a]]
shrinkList xs = do
 concatMap
   (\k -> removes k xs)
   (halves $ length xs)

halves :: Integral a => a -> [a]
halves =
  takeWhile (/= 0) . iterate (`quot` 2)

removes :: Int -> [a] -> [[a]]
removes k0 xs0 =
  let
    loop k n xs =
      let
        (hd, tl) =
          splitAt k xs
      in
        if k > n then
          []
        else if null tl then
          [[]]
        else
          tl : fmap (hd ++) (loop k (n - k) tl)
  in
    loop k0 (length xs0) xs0

------------------------------------------------------------------------
-- Combinators - Shrinking

shrink :: (a -> [a]) -> Gen a -> Gen a
shrink =
  mapGen . fmap . expandTree

prune :: Gen a -> Gen a
prune =
  mapGen (fmap pruneTree)

------------------------------------------------------------------------
-- Combinators - Range

integral_ :: Integral a => a -> a -> Gen a
integral_ lo hi =
  Gen $
    pure . pure . fromInteger . fst .
      nextInteger (toInteger lo) (toInteger hi)

integral :: Integral a => a -> a -> Gen a
integral lo hi =
  shrink (towards lo) $ integral_ lo hi

enum :: Enum a => a -> a -> Gen a
enum lo hi =
  fmap toEnum $ integral (fromEnum lo) (fromEnum hi)

------------------------------------------------------------------------
-- Combinators - Choice

element :: [a] -> Gen a
element [] = error "Rose.element: used with empty list"
element xs = do
  n <- integral 0 (length xs - 1)
  return $ xs !! n

choice :: [Gen a] -> Gen a
choice [] = error "Rose.choice: used with empty list"
choice xs = do
  n <- integral 0 (length xs - 1)
  xs !! n

----------------------------------------------------------------------
-- Combinators - Conditional

filter :: (a -> Bool) -> Gen a -> Gen a
filter p gen =
  let
    loop = \case
      0 ->
        empty
      n ->
        mfilter p gen <|> loop (n - 1)
  in
    loop (100 :: Int)

just :: Gen (Maybe a) -> Gen a
just =
  fmap fromJust . filter isJust

----------------------------------------------------------------------
-- Combinators - Collections

list :: Int -> Int -> Gen a -> Gen [a]
list lo hi gen =
  filter ((>= lo) . length) .
  shrink shrinkList $ do
    k <- integral_ lo hi
    replicateM k gen

----------------------------------------------------------------------
-- Tree - Rendering

--
-- Rendering implementation based on the one from containers/Data.Tree
--

renderTreeLines :: Tree String -> [String]
renderTreeLines (Node x xs) =
  lines (renderNode x) ++ renderForestLines xs

renderNode :: String -> String
renderNode xs =
  case xs of
    [_] ->
      ' ' : xs
    _ ->
      xs

renderForestLines :: [Tree String] -> [String]
renderForestLines xs0 =
  let
    shift first other =
      zipWith (++) (first : repeat other)
  in
    case xs0 of
      [] ->
        []

      [x] -> do
        shift " └╼" "   " (renderTreeLines x)

      x : xs ->
        shift " ├╼" " │ " (renderTreeLines x) ++
        renderForestLines xs

renderTree :: Tree String -> String
renderTree =
  unlines . renderTreeLines

------------------------------------------------------------------------
-- Sampling

printDiscard :: (Tree a -> IO ()) -> Maybe (Tree a) -> IO ()
printDiscard io mxs =
  case mxs of
    Nothing ->
      putStrLn "<Discard>"
    Just xs ->
      io xs

printSample :: Show a => Gen a -> IO ()
printSample gen = do
  xs0 <- sampleGen gen
  flip printDiscard xs0 $ \(Node x0 xs) -> do
    putStrLn "=== Outcome ==="
    putStrLn $ show x0
    putStrLn "=== Shrinks ==="
    for_ xs $ \(Node x _) ->
      putStrLn $ show x

printSampleTree :: Show a => Gen a -> IO ()
printSampleTree arb =
  printDiscard (putStr . renderTree . fmap show) =<< sampleGen arb

printSampleTree' :: Show a => Int -> Gen a -> IO ()
printSampleTree' seed =
  printDiscard (putStr . renderTree . fmap show) . runGen (mkSeed seed)

------------------------------------------------------------------------
-- Property

newtype Shrinks =
  Shrinks Int
  deriving (Show)

data Status =
    Failed Shrinks [String]
  | GaveUp
  | OK
    deriving (Show)

data Report =
  Report {
      reportTests :: Int
    , reportDiscards :: Int
    , reportStatus :: Status
    } deriving (Show)

newtype Property a =
  Property (MaybeT (WriterT [String] Gen) a)
  deriving (Functor, Applicative, Monad)

runProperty :: Property a -> Gen (Maybe a, [String])
runProperty (Property p) =
  runWriterT $ runMaybeT p

counterexample :: String -> Property ()
counterexample =
  Property . lift . tell . pure

discard :: Property a
discard =
  Property . lift $ lift empty

failure :: Property a
failure =
  Property . MaybeT $ pure Nothing

success :: Property ()
success =
  Property $ pure ()

assert :: Bool -> Property ()
assert b =
  if b then
    success
  else
    failure

infix 4 ===

(===) :: (Eq a, Show a) => a -> a -> Property ()
(===) x y =
  if x == y then
    success
  else do
    counterexample "=== Not Equal ==="
    counterexample (show x)
    counterexample (show y)
    failure

forAll :: Show a => Gen a -> Property a
forAll gen = do
  x <- Property . lift $ lift gen
  counterexample (show x)
  pure x

find :: [a] -> b -> (a -> Maybe b) -> b
find xs0 def p =
  case xs0 of
    [] ->
      def
    x0 : xs ->
      case p x0 of
        Nothing ->
          find xs def p
        Just x ->
          x

isFailure :: Tree (Maybe a, b) -> Bool
isFailure = \case
  Node (Nothing, _) _ ->
    True
  _ ->
    False

takeSmallest :: Shrinks -> Tree (Maybe (), [String]) -> Status
takeSmallest (Shrinks n) (Node (x, w) xs) =
  case x of
    Nothing ->
      find xs (Failed (Shrinks n) w) $ \node ->
        if isFailure node then
          Just $ takeSmallest (Shrinks $ n + 1) node
        else
          Nothing

    Just () ->
      OK

checkN :: Int -> Property () -> IO Report
checkN n prop =
  let
    loop :: Int -> Int -> Seed -> Report
    loop !tests !discards !seed =
      if tests == n then
        Report tests discards OK
      else if discards >= 100 then
        Report tests discards GaveUp
      else
        case splitSeed seed of
          (s0, s1) ->
            case runGen s0 (runProperty prop) of
              Nothing ->
                loop tests (discards + 1) s1

              Just t ->
                if isFailure t then
                  Report (tests + 1) discards $ takeSmallest (Shrinks 0) t
                else
                  loop (tests + 1) discards s1
  in
    loop 0 0 <$> newSeed

renderTests :: Int -> String
renderTests = \case
  1 ->
    "1 test"
  n ->
    show n ++ " tests"

renderDiscards :: Int -> String
renderDiscards = \case
  1 ->
    "1 discard"
  n ->
    show n ++ " discards"

renderAndDiscards :: Int -> String
renderAndDiscards = \case
  0 ->
    ""
  1 ->
    " and 1 discard"
  n ->
    " and " ++ show n ++ " discards"

renderAndShrinks :: Shrinks -> String
renderAndShrinks = \case
  Shrinks 0 ->
    ""
  Shrinks 1 ->
    " and 1 shrink"
  Shrinks n ->
    " and " ++ show n ++ " shrinks"

check :: Property () -> IO ()
check prop = do
  Report tests discards status <- checkN 100 prop
  case status of
    Failed shr msgs -> do
      putStrLn $
        "*** Failed! Falsifiable (after " ++
        renderTests tests ++
        renderAndShrinks shr ++
        renderAndDiscards discards ++
        "):"
      putStr $ unlines msgs
    GaveUp ->
      putStrLn $
        "*** Gave up after " ++
        renderDiscards discards ++
        ", passed " ++
        renderTests tests ++
        "."
    OK ->
      putStrLn $
        "+++ OK, passed " ++
        renderTests tests ++
        "."

------------------------------------------------------------------------
-- Example

newtype Name =
  Name String
  deriving (Eq, Ord, Show)

newtype USD =
  USD Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

data Item =
  Item Name USD
  deriving (Eq, Ord, Show)

newtype Order =
  Order [Item]
  deriving (Eq, Ord, Show)

merge :: Order -> Order -> Order
merge (Order xs) (Order ys) =
  Order $ xs ++ ys ++
    if any ((> 50) . price) xs ||
       any ((> 50) . price) ys then
      [Item (Name "processing") (USD 1)]
    else
      []

price :: Item -> USD
price (Item _ x) =
  x

total :: Order -> USD
total (Order xs) =
  sum $ fmap price xs

cheap :: Gen Item
cheap =
  Item
    <$> (Name <$> element ["sandwich", "noodles"])
    <*> (USD <$> integral 5 10)

expensive :: Gen Item
expensive =
  Item
    <$> (Name <$> element ["oculus", "vive"])
    <*> (USD <$> integral 1000 2000)

order :: Gen Item -> Gen Order
order gen =
  Order <$> list 0 50 gen

-- | Fails with:
--
-- @
-- λ check $ prop_total
-- *** Failed! Falsifiable (after 1 test and 113 shrinks):
-- Order []
-- Order [Item (Name "oculus") (USD 1000)]
-- === Not Equal ===
-- USD 1001
-- USD 1000
-- @
--
prop_total :: Property ()
prop_total = do
  x <- forAll (order cheap)
  y <- forAll (order expensive)
  total (merge x y) === total x + total y
