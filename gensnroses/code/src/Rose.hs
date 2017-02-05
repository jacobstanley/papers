{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Rose where

import           Control.Monad (liftM, ap, forM_, when)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Writer.Lazy (WriterT(..), tell)

import           Data.Bifunctor (bimap, second)
import           Data.Functor.Classes (Show1(..), showsPrec1)
import           Data.Functor.Classes (showsUnaryWith, showsBinaryWith)

import qualified System.Random as Random


------------------------------------------------------------------------
-- Seed

newtype Seed =
  Seed Random.StdGen

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
-- GenT

newtype GenT m a =
  GenT (Seed -> m a)

runGenT :: Seed -> GenT m a -> m a
runGenT seed (GenT m) =
  m seed

mapGenT :: (m a -> n b) -> GenT m a -> GenT n b
mapGenT f (GenT m) =
  GenT $ \s ->
    f (m s)

sampleGenT :: MonadIO m => GenT m a -> m a
sampleGenT gen = do
  seed <- liftIO newSeed
  runGenT seed gen

instance Monad m => Monad (GenT m) where
  return =
    GenT . const . return

  (>>=) m k =
    GenT $ \s ->
      case splitSeed s of
        (sk, sm) ->
          runGenT sk . k =<<
          runGenT sm m

instance MonadTrans GenT where
  lift =
    GenT . const

instance MFunctor GenT where
  hoist =
    mapGenT

instance MonadIO m => MonadIO (GenT m) where
  liftIO =
    lift . liftIO

instance Monad m => Functor (GenT m) where
  fmap =
    liftM

instance Monad m => Applicative (GenT m) where
  pure =
    return
  (<*>) =
    ap

------------------------------------------------------------------------
-- TreeT

data Node m a =
  Node a [TreeT m a]

newtype TreeT m a =
  TreeT (m (Node m a))

runTreeT :: TreeT m a -> m (Node m a)
runTreeT (TreeT m) =
  m

instance Monad m => Functor (Node m) where
  fmap f (Node x xs) =
    Node (f x) (fmap (fmap f) xs)

instance Monad m => Monad (TreeT m) where
  return x =
    TreeT $
      return (Node x [])

  (>>=) m k =
    TreeT $ do
      Node x xs <- runTreeT m
      Node y ys <- runTreeT (k x)
      return . Node y $
        fmap (>>= k) xs ++ ys

instance MonadTrans TreeT where
  lift m =
    TreeT $ do
      x <- m
      return (Node x [])

instance MFunctor TreeT where
  hoist f (TreeT m) =
    let
      hoistNode (Node x xs) =
        Node x (fmap (hoist f) xs)
    in
      TreeT . f $ fmap hoistNode m

instance MonadIO m => MonadIO (TreeT m) where
  liftIO =
    lift . liftIO

instance Monad m => Functor (TreeT m) where
  fmap =
    liftM

instance Monad m => Applicative (TreeT m) where
  pure =
    return
  (<*>) =
    ap

unfoldTree :: Monad m => (b -> a) -> (b -> [b]) -> b -> TreeT m a
unfoldTree f g x =
  TreeT . return $
    Node (f x) (unfoldForest f g x)

unfoldForest :: Monad m => (b -> a) -> (b -> [b]) -> b -> [TreeT m a]
unfoldForest f g =
  fmap (unfoldTree f g) . g

expandTree :: Monad m => (a -> [a]) -> TreeT m a -> TreeT m a
expandTree f m =
  TreeT $ do
    Node x xs <- runTreeT m
    return . Node x $
      fmap (expandTree f) xs ++ unfoldForest id f x

pruneTree :: Monad m => TreeT m a -> TreeT m a
pruneTree (TreeT m) =
  TreeT $ do
    Node x _ <- m
    return $ Node x []

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

halves :: Integral a => a -> [a]
halves =
  takeWhile (/= 0) . iterate (`quot` 2)

------------------------------------------------------------------------
-- Combinators - Shrinking

shrink :: Monad m => (a -> [a]) -> GenT (TreeT m) a -> GenT (TreeT m) a
shrink =
  mapGenT . expandTree

noShrink :: Monad m => GenT (TreeT m) a -> GenT (TreeT m) a
noShrink =
  mapGenT pruneTree

------------------------------------------------------------------------
-- Combinators - Ranges

integral_ :: (Monad m, Integral a) => a -> a -> GenT m a
integral_ lo hi =
  GenT $
    return . fromInteger . fst .
      nextInteger (toInteger lo) (toInteger hi)

integral :: (Monad m, Integral a) => a -> a -> GenT (TreeT m) a
integral lo hi =
  shrink (towards lo) $ integral_ lo hi

enum :: (Monad m, Enum a) => a -> a -> GenT (TreeT m) a
enum lo hi =
  fmap toEnum $ integral (fromEnum lo) (fromEnum hi)

element :: Monad m => [a] -> GenT (TreeT m) a
element [] = error "Rose.element: used with empty list"
element xs = do
  n <- integral 0 (length xs - 1)
  return $ xs !! n

choice :: Monad m => [GenT (TreeT m) a] -> GenT (TreeT m) a
choice [] = error "Rose.choice: used with empty list"
choice xs = do
  n <- integral 0 (length xs - 1)
  xs !! n

------------------------------------------------------------------------
-- Tree - Show/Show1 instances, rendering

instance (Show1 m, Show a) => Show (Node m a) where
  showsPrec =
    showsPrec1

instance (Show1 m, Show a) => Show (TreeT m a) where
  showsPrec =
    showsPrec1

instance Show1 m => Show1 (Node m) where
  liftShowsPrec sp sl d (Node x xs) =
    let
      sp1 =
        liftShowsPrec sp sl

      sl1 =
        liftShowList sp sl

      sp2 =
        liftShowsPrec sp1 sl1
    in
      showsBinaryWith sp sp2 "Node" d x xs

instance Show1 m => Show1 (TreeT m) where
  liftShowsPrec sp sl d (TreeT m) =
    let
      sp1 =
        liftShowsPrec sp sl

      sl1 =
        liftShowList sp sl

      sp2 =
        liftShowsPrec sp1 sl1
    in
      showsUnaryWith sp2 "TreeT" d m

--
-- Rendering implementation based on the one from containers/Data.Tree
--

renderTreeLines :: Monad m => TreeT m String -> m [String]
renderTreeLines (TreeT m) = do
  Node x xs0 <- m
  xs <- renderForestLines xs0
  return $
    lines (renderNode x) ++ xs

renderNode :: String -> String
renderNode xs =
  case xs of
    [_] ->
      ' ' : xs
    _ ->
      xs

renderForestLines :: Monad m => [TreeT m String] -> m [String]
renderForestLines xs0 =
  let
    shift first other =
      zipWith (++) (first : repeat other)
  in
    case xs0 of
      [] ->
        return []

      [x] -> do
        s <- renderTreeLines x
        return $
          shift " └╼" "   " s

      x : xs -> do
        s <- renderTreeLines x
        ss <- renderForestLines xs
        return $
          shift " ├╼" " │ " s ++ ss

renderTree :: Monad m => TreeT m String -> m String
renderTree =
  fmap unlines . renderTreeLines

------------------------------------------------------------------------
-- Sampling

printSample :: Show a => GenT (TreeT IO) a -> IO ()
printSample gen = do
  Node x ts <- runTreeT $ sampleGenT gen
  putStrLn "=== Outcome ==="
  putStrLn $ show x
  putStrLn "=== Shrinks ==="
  forM_ ts $ \t -> do
    Node y _ <- runTreeT t
    putStrLn $ show y

printSampleTree :: Show a => GenT (TreeT IO) a -> IO ()
printSampleTree =
  (putStr =<<) . renderTree . fmap show . sampleGenT

printSampleTree' :: Show a => Int -> GenT (TreeT IO) a -> IO ()
printSampleTree' seed =
  (putStr =<<) . renderTree . fmap show . runGenT (Seed $ Random.mkStdGen seed)

------------------------------------------------------------------------
-- Property

data Break =
    Failure
  | Discard
    deriving (Show)

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

type Property =
  forall m. Monad m => PropertyT m ()

newtype PropertyT m a =
  PropertyT {
      unPropertyT :: GenT (ExceptT Break (WriterT [String] (TreeT m))) a
    } deriving (Functor, Applicative, Monad)

runPropertyT :: PropertyT m a -> GenT m (Node m (Either Break a, [String]))
runPropertyT (PropertyT p) =
  mapGenT (runTreeT . runWriterT . runExceptT) p

forAll :: (Monad m, Show a) => GenT (TreeT m) a -> PropertyT m a
forAll gen = do
  x <- PropertyT $ hoist (lift . lift) gen
  counterexample (show x)
  return x

counterexample :: Monad m => String -> PropertyT m ()
counterexample =
  PropertyT . lift . lift . tell . pure

discard :: Monad m => PropertyT m a
discard =
  PropertyT . lift $
    throwE Discard

failure :: Monad m => PropertyT m a
failure =
  PropertyT . lift $
    throwE Failure

success :: Monad m => PropertyT m ()
success =
  PropertyT $
    pure ()

assert :: Monad m => Bool -> PropertyT m ()
assert b =
  if b then
    success
  else
    failure

findM :: Monad m => [a] -> b -> (a -> m (Maybe b)) -> m b
findM xs0 def p =
  case xs0 of
    [] ->
      return def
    x0 : xs ->
      p x0 >>= \m ->
        case m of
          Nothing ->
            findM xs def p
          Just x ->
            return x

isFailure :: Node m (Either Break a, b) -> Bool
isFailure (Node (x, _) _) =
  case x of
    Left Failure ->
      True
    _ ->
      False

takeSmallest :: Monad m => Shrinks -> Node m (Either Break (), [String]) -> m Status
takeSmallest (Shrinks n) (Node (x, w) xs) =
  case x of
    Left Failure ->
      findM xs (Failed (Shrinks n) w) $ \(TreeT m) -> do
        node <- m
        if isFailure node then
          Just <$> takeSmallest (Shrinks $ n + 1) node
        else
          return Nothing

    Left Discard ->
      return GaveUp

    Right () ->
      return OK

report :: forall m. Monad m => Int -> PropertyT m () -> GenT m Report
report n p =
  let
    loop :: Int -> Int -> GenT m Report
    loop !tests !discards =
      if tests == n then
        pure $ Report tests discards OK
      else if discards >= 100 then
        pure $ Report tests discards GaveUp
      else do
        node@(Node (x, _) _) <- runPropertyT p
        case x of
          Left Failure ->
            Report tests discards <$> lift (takeSmallest (Shrinks 0) node)

          Left Discard ->
            loop tests (discards + 1)

          Right () ->
            loop (tests + 1) discards
  in
    loop 0 0

check :: MonadIO m => PropertyT m () -> m Report
check p = do
  seed <- liftIO newSeed
  runGenT seed $
    report 100 p

-- Try 'check prop_foo' to see what happens
prop_foo :: Property
prop_foo = do
  x <- forAll $ enum 'a' 'z'
  y <- forAll $ integral 0 50

  when (y `mod` 2 == (0 :: Int))
    discard

  assert $ y < 87 && x <= 'r'
