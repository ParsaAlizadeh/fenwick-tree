{-# LANGUAGE BangPatterns #-}

module Data.Fenwick.Array 
  ( FenMArray
  , newFen
  , newAccumFen
  , newListFen
  , getSizeFen
  , addFen
  , sumPrefixFen
  , lowerBoundFen
  ) where

import Data.Array.MArray
import Control.Monad
import Data.Bits
import Data.Semigroup.Cancellative
import Data.Monoid.Cancellative
import Data.Maybe
import Debug.Trace

data FenMArray array elem = FenMArray Int (array Int elem)

lsb :: Int -> Int
lsb node = node .&. (-node)
{-# INLINE lsb #-}

modifyArray' :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray' arr i f = do
  x <- readArray arr i
  let !x' = f x
  writeArray arr i x'

newFen :: (MArray array elem m, CommutativeMonoid elem) => Int -> m (FenMArray array elem)
newFen n = do
  arr <- newArray (1, n) mempty
  pure (FenMArray n arr)
{-# INLINABLE newFen #-}

newAccumFen :: (MArray array elem m, Monoid elem, Foldable t) => Int -> t (Int, elem) -> m (FenMArray array elem)
newAccumFen n xs = do
  arr <- newArray (1, n) mempty
  forM_ xs $ \(i, e) -> do
    modifyArray' arr i (<> e)
  forM_ [1..n] $ \i -> do
    let j = i + lsb i
    when (j <= n) $ do
      e <- readArray arr i
      modifyArray' arr j (e <>)
  pure (FenMArray n arr)
{-# INLINABLE newAccumFen #-}

newListFen :: (MArray array elem m, Monoid elem) => Int -> [elem] -> m (FenMArray array elem)
newListFen n xs = newAccumFen n $ zip [1..n] xs
{-# INLINABLE newListFen #-}

getSizeFen :: FenMArray array elem -> Int
getSizeFen (FenMArray n _) = n

addFen :: (MArray array elem f, Commutative elem) => FenMArray array elem -> Int -> elem -> f ()
addFen (FenMArray n arr) r a = go r where
  -- 1 <= r <= n
  go i = when (i <= n) $ do
    modifyArray' arr i (a <>)
    go (i + lsb i)
{-# INLINABLE addFen #-}

sumPrefixFen :: (MArray array elem f, CommutativeMonoid elem) => FenMArray array elem -> Int -> f elem
sumPrefixFen (FenMArray n arr) = go mempty where
  -- prefix (0, r], for 1 <= r <= n
  go !s i
    | i <= 0 = pure s
    | otherwise = do
      x <- readArray arr i
      go (x <> s) (i - lsb i)
{-# INLINABLE sumPrefixFen #-}

-- only applicable when partial sums are sorted
lowerBoundFen :: (MArray array elem m, CommutativeMonoid elem, Ord elem) => FenMArray array elem -> elem -> m Int
lowerBoundFen (FenMArray n arr) query = go root (n + 1) mempty where
  root = bit (finiteBitSize n - countLeadingZeros n - 1)
  go node
    | odd node = leaf node
    | otherwise = nonleaf node
  leaf node fallback prepend 
    | node > n = pure fallback
    | otherwise =  do
    nodeval <- readArray arr node
    pure $ if prepend <> nodeval < query
      then fallback
      else node 
  nonleaf node fallback prepend 
    | node > n = go (leftOf node) fallback prepend
    | otherwise = do
    nodeval <- readArray arr node
    if prepend <> nodeval < query
      then go (rightOf node) fallback (prepend <> nodeval)
      else go (leftOf node) node prepend
  leftOf node = node - (lsb node `unsafeShiftR` 1)
  rightOf node = node + (lsb node `unsafeShiftR` 1)
{-# INLINABLE lowerBoundFen #-}
