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
import Data.Array.Base
import GHC.Ix

data FenMArray array elem = FenMArray Int (array Int elem)

lsb :: Int -> Int
lsb node = node .&. (-node)
{-# INLINE lsb #-}

unsafeModifyArray' :: (MArray a e m, Ix i) => a i e -> Int -> (e -> e) -> m ()
unsafeModifyArray' arr i f = do
  x <- unsafeRead arr i
  let !x' = f x
  unsafeWrite arr i x'
{-# INLINABLE unsafeModifyArray' #-}

newFen :: (MArray array elem m, Monoid elem) => Int -> m (FenMArray array elem)
newFen n = do
  arr <- newArray (0, n) mempty
  pure (FenMArray n arr)
{-# INLINABLE newFen #-}

newAccumFen :: (MArray array elem m, CommutativeMonoid elem, Foldable t) => Int -> t (Int, elem) -> m (FenMArray array elem)
newAccumFen n xs = do
  arr <- newArray (0, n) mempty
  forM_ xs $ \(i, e) -> do
    unsafeModifyArray' arr i (<> e)
  forM_ [1..n] $ \i -> do
    let j = i + lsb i
    when (j <= n) $ do
      e <- readArray arr i
      unsafeModifyArray' arr j (e <>)
  pure (FenMArray n arr)
{-# INLINABLE newAccumFen #-}

newListFen :: (MArray array elem m, CommutativeMonoid elem) => Int -> [elem] -> m (FenMArray array elem)
newListFen n xs = newAccumFen n $ zip [1..n] xs
{-# INLINABLE newListFen #-}

getSizeFen :: FenMArray array elem -> Int
getSizeFen (FenMArray n _) = n
{-# INLINE getSizeFen #-}

addFen :: (MArray array elem m, Commutative elem) => FenMArray array elem -> Int -> elem -> m ()
addFen (FenMArray n arr) r a = go r where
  -- 1 <= r <= n
  go i = when (i <= n) $ do
    unsafeModifyArray' arr i (a <>)
    go (i + lsb i)
{-# INLINABLE addFen #-}

sumPrefixFen :: (MArray array elem m, Monoid elem) => FenMArray array elem -> Int -> m elem
sumPrefixFen (FenMArray n arr) = go mempty where
  -- prefix (0, r], for 1 <= r <= n
  go !s i
    | i <= 0 = pure s
    | otherwise = do
      x <- unsafeRead arr i
      go (x <> s) (i - lsb i)
{-# INLINABLE sumPrefixFen #-}

-- only applicable when partial sums are sorted
lowerBoundFen :: (MArray array elem m, Monoid elem, Ord elem) => FenMArray array elem -> elem -> m Int
lowerBoundFen (FenMArray n arr) query = go root (n + 1) mempty where
  root = bit (finiteBitSize n - countLeadingZeros n - 1)
  go node
    | odd node = leaf node
    | otherwise = nonleaf node
  leaf node fallback prepend 
    | node > n = pure fallback
    | otherwise = do
    nodeval <- unsafeRead arr node
    pure $ if prepend <> nodeval < query
      then fallback
      else node 
  nonleaf node fallback prepend 
    | node > n = go (leftOf node) fallback prepend
    | otherwise = do
    nodeval <- unsafeRead arr node
    if prepend <> nodeval < query
      then go (rightOf node) fallback (prepend <> nodeval)
      else go (leftOf node) node prepend
  leftOf node = node - (lsb node `unsafeShiftR` 1)
  rightOf node = node + (lsb node `unsafeShiftR` 1)
{-# INLINABLE lowerBoundFen #-}
