{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
module Rose where

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..), liftM, ap, mfilter, join, replicateM)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Control.Monad.Trans.Writer.Lazy (WriterT(..), tell)

import           Data.Bifunctor (bimap, second)
import           Data.Foldable (for_, toList)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe, fromJust, isJust)

import           Prelude hiding (filter)

import qualified System.Random as Random

import           Text.Printf (printf)


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

bindMaybeTree :: (a -> Maybe (Tree b)) -> Maybe (Tree a) -> Maybe (Tree b)
bindMaybeTree k m =
  fmap join . mapMaybeTree k =<< m

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

liftTree :: Maybe (Tree a) -> Gen a
liftTree x =
  Gen (\_ -> x)

freeze :: Gen a -> Gen (a, Gen a)
freeze gen =
  Gen $ \seed -> do
    Node x xs <- runGen seed gen
    Just $ Node (x, liftTree $ Just (Node x xs)) []

instance Monad Gen where
  return =
    liftTree . pure . pure

  (>>=) m k =
    Gen $ \s ->
      case splitSeed s of
        (sk, sm) ->
          (runGen sk . k) `bindMaybeTree`
          (runGen sm m)

instance MonadPlus Gen where
  mzero =
    liftTree Nothing

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

int :: Int -> Int -> Gen Int
int =
  integral

enum :: Enum a => a -> a -> Gen a
enum lo hi =
  fmap toEnum $ integral (fromEnum lo) (fromEnum hi)

------------------------------------------------------------------------
-- Combinators - Choice

elements :: [a] -> Gen a
elements [] = error "Rose.elements: used with empty list"
elements xs = do
  n <- integral 0 (length xs - 1)
  pure $ xs !! n

oneof :: [Gen a] -> Gen a
oneof [] = error "Rose.oneof: used with empty list"
oneof xs = do
  n <- integral 0 (length xs - 1)
  xs !! n

----------------------------------------------------------------------
-- Combinators - Conditional

suchThat :: Gen a -> (a -> Bool) -> Gen a
suchThat gen p =
  let
    loop = \case
      0 ->
        empty
      n ->
        mfilter p gen <|> loop (n - 1)
  in
    loop (100 :: Int)

justOf :: Gen (Maybe a) -> Gen a
justOf gen =
  fmap fromJust (gen `suchThat` isJust)

----------------------------------------------------------------------
-- Combinators - Collections

listOf :: Int -> Int -> Gen a -> Gen [a]
listOf lo hi gen =
  (sequence =<<) .
  mfilter ((>= lo) . length) .
  shrink shrinkList $ do
    k <- integral_ lo hi
    replicateM k (fmap snd $ freeze gen)

------------------------------------------------------------------------
-- Combinators - Subterms

data Subterms n a =
    One a
  | All (Vec n a)
    deriving (Functor, Foldable, Traversable)

data Nat =
    Z
  | S Nat

data Vec n a where
  Nil :: Vec 'Z a
  (:.) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :.

deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)
deriving instance Traversable (Vec n)

shrinkSubterms :: Subterms n a -> [Subterms n a]
shrinkSubterms = \case
  One _ ->
    []
  All xs ->
    fmap One $ toList xs

subterms :: Vec n (Gen a) -> Gen (Subterms n a)
subterms =
  (sequence =<<) .
  shrink shrinkSubterms .
  fmap All .
  mapM (fmap snd . freeze)

fromSubterms :: Applicative m => (Vec n a -> m a) -> Subterms n a -> m a
fromSubterms f = \case
  One x ->
    pure x
  All xs ->
    f xs

withSubterms :: Vec n (Gen a) -> (Vec n a -> Gen a) -> Gen a
withSubterms gs f =
  fromSubterms f =<< subterms gs

liftS :: (a -> a) -> Gen a -> Gen a
liftS f gx =
  withSubterms (gx :. Nil) $ \(x :. Nil) ->
    pure (f x)

liftS2 :: (a -> a -> a) -> Gen a -> Gen a -> Gen a
liftS2 f gx gy =
  withSubterms (gx :. gy :. Nil) $ \(x :. y :. Nil) ->
    pure (f x y)

liftS3 :: (a -> a -> a -> a) -> Gen a -> Gen a -> Gen a -> Gen a
liftS3 f gx gy gz =
  withSubterms (gx :. gy :. gz :. Nil) $ \(x :. y :. z :. Nil) ->
    pure (f x y z)

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

              Just t | isFailure t ->
                Report (tests + 1) discards $ takeSmallest (Shrinks 0) t

              Just _ ->
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

data Aggregate =
    Minimum
  | Maximum
  | Sum
    deriving (Eq, Ord, Show)

data Schema =
    SInt Aggregate
  | STuple Schema Schema
    deriving (Eq, Ord, Show)

data Value =
    VInt Int
  | VTuple Value Value
    deriving (Eq, Ord, Show)

checkValue :: Schema -> Value -> Bool
checkValue schema x0 =
  case schema of
    SInt _
      | VInt _ <- x0
      ->
        True

    STuple ls rs
      | VTuple lx rx <- x0
      ->
        checkValue ls lx &&
        checkValue rs rx

    _ ->
      False

mergeInt :: Aggregate -> Int -> Int -> Int
mergeInt aggregate =
  case aggregate of
    Minimum ->
      min
    Maximum ->
      max
    Sum ->
      (+)

mergeValue :: Schema -> Value -> Value -> Value
mergeValue schema x0 y0 =
  case schema of
    SInt aggregate
      | VInt x <- x0
      , VInt y <- y0
      ->
        VInt (mergeInt aggregate x y)

    STuple ls rs
      | VTuple lx rx <- x0
      , VTuple ly ry <- y0
      ->
        VTuple (mergeValue rs rx ry) (mergeValue ls lx ly)

    _ ->
      error $ "Schema mismatch " ++ show (schema, x0, y0)

genAggregate :: Gen Aggregate
genAggregate =
  elements [Minimum, Maximum, Sum]

genSchema :: Gen Aggregate -> Gen Schema
genSchema gen =
  let
    loop n =
      if n <= 1 then
        SInt <$> gen
      else
        oneof [
            SInt <$> gen
          , liftS2 STuple
              (loop (n `div` 2))
              (loop (n `div` 2))
          ]
  in
    loop (100 :: Int)

genValue :: Schema -> Gen Value
genValue = \case
  SInt _ ->
    VInt <$> integral 0 100
  STuple sx sy ->
    VTuple <$> genValue sx <*> genValue sy

prop_merge_value :: Property ()
prop_merge_value = do
  schema <- forAll $ genSchema genAggregate
  v0 <- forAll $ genValue schema
  v1 <- forAll $ genValue schema

  let merged = mergeValue schema v0 v1

  counterexample $ show merged
  assert $ checkValue schema merged

mergeCompare :: Gen Aggregate -> (Value -> Value -> Bool) -> Property ()
mergeCompare gen cmp = do
  schema <- forAll $ genSchema gen
  v0 <- forAll $ genValue schema
  v1 <- forAll $ genValue schema

  let merged = mergeValue schema v0 v1
  counterexample $ show merged

  assert $
    cmp merged v0 &&
    cmp merged v1

prop_merge_minimum :: Property ()
prop_merge_minimum = do
  mergeCompare (pure Minimum) (<=)

prop_merge_maximum :: Property ()
prop_merge_maximum = do
  mergeCompare (pure Maximum) (>=)

prop_merge_sum :: Property ()
prop_merge_sum = do
  mergeCompare (pure Sum) (>=)

------------------------------------------------------------------------
-- Example Extra (not sure if this adds value)

mergeFile :: Schema -> [(String, Value)] -> [(String, Value)] -> [(String, Value)]
mergeFile schema xs0 ys0 =
  case (xs0, ys0) of
    ([], _) ->
      ys0

    (_, []) ->
      xs0

    ((nx, vx) : xs, (ny, vy) : ys)
      | nx == ny
      ->
        (nx, mergeValue schema vx vy) : mergeFile schema xs ys

      | nx < ny
      ->
        (nx, vx) : mergeFile schema xs ys0

      | otherwise
      ->
        (ny, vy) : mergeFile schema xs0 ys

genCustomerId :: Gen String
genCustomerId =
  printf "C+%04d" <$> int 0 10

genFile :: Schema -> Gen [(String, Value)]
genFile schema =
  fmap (Map.toList . Map.fromList) . listOf 1 10 $
    (,) <$> genCustomerId <*> genValue schema

prop_merge_file :: Property ()
prop_merge_file = do
  schema <- forAll $ genSchema genAggregate
  fileX <- forAll (genFile schema)
  fileY <- forAll (genFile schema)

  let merged = mergeFile schema fileX fileY

  counterexample $ show merged
  assert $ all (checkValue schema) (fmap snd merged)
