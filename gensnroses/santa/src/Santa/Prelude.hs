module Santa.Prelude (
    module X
  , hoistEither
  ) where

import           Control.Monad as X (Monad(..), MonadPlus(..))
import           Control.Monad as X ((=<<), (>=>), (<=<), forever)
import           Control.Monad as X (join, mfilter, filterM, mapAndUnzipM, zipWithM, zipWithM_)
import           Control.Monad as X (foldM, foldM_, replicateM, replicateM_)
import           Control.Monad as X (guard, when, unless)
import           Control.Monad as X (liftM, liftM2, liftM3, liftM4, liftM5, ap)
import           Control.Monad.IO.Class as X (
                     liftIO
                   )
import           Control.Monad.Trans.Except as X (
                     ExceptT (..)
                   , runExceptT
                   , throwE
                   )

import           Control.Applicative as X (
                     Applicative(..)
                   , Alternative(..)
                   , Const(..)
                   , WrappedMonad(..)
                   , WrappedArrow(..)
                   , ZipList(..)
                   , (<**>)
                   , liftA
                   , liftA2
                   , liftA3
                   , optional
                   )
import           Data.Eq as X
import           Data.Bifunctor as X (Bifunctor(..))
import           Data.Bool as X
import           Data.Char as X (Char)
import           Data.Function as X (
                      id
                    , const
                    , (.)
                    , flip
                    , ($)
                    , fix
                    , on
                    )
import           Data.Functor as X (
                      Functor(..)
                   , ($>)
                   , (<$>)
                   , void
                   )
import           Data.List as X (
                     intercalate
                   , isPrefixOf
                   , drop
                   , splitAt
                   , break
                   , filter
                   , reverse
                   )
import           Data.Maybe as X hiding (fromJust)
import           Data.Monoid as X (
                     All(..)
                   , Any(..)
                   , First(..)
                   , Last(..)
                   , Monoid(..)
                   , Dual(..)
                   , Endo(..)
                   , (<>)
                   , Sum(..)
                   , Product(..)
                   )
import           Data.Either as X
import           Data.Int as X
import           Data.Ord as X
import           Data.String as X (
                     String
                   )
import           Data.Tuple as X
import           Data.Traversable as X
import           Data.Foldable as X hiding (
                     foldr1
                   , foldl1
                   , maximum
                   , maximumBy
                   , minimum
                   , minimumBy
                   )

import           GHC.Num as X
import           GHC.Real as X
import           GHC.Float as X

import           Prelude as X (
                     Enum
                   , Bounded
                   , minBound
                   , maxBound
                   , ($!)
                   , seq
                   )

import           System.IO as X (IO)

import           Text.Show as X
import           Text.Read as X (Read, reads, readMaybe, readEither)

hoistEither :: Monad m => Either l r -> ExceptT l m r
hoistEither =
  either throwE pure
