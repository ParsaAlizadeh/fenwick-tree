{-# LANGUAGE BangPatterns #-}

module Data.Fenwick.Vector 
  ( FenMVector
  , newFen
  , newAccumFen
  , newListFen
  , getSizeFen
  , addFen
  , sumPrefixFen
  , lowerBoundFen
  ) where

import Control.Monad
import Data.Bits
import Data.Semigroup.Commutative
import Data.Monoid.Cancellative
import qualified Data.Vector.Generic.Mutable as V

data FenMVector vector s elem = FenMVector Int (vector s elem)

lsb :: Int -> Int
lsb node = node .&. (-node)
{-# INLINE lsb #-}

unsafeModify' :: (V.PrimMonad m, V.MVector v a) => v (V.PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModify' vec f = V.unsafeModify vec (f $!)
{-# INLINABLE unsafeModify' #-}

newFen :: (V.MVector vector elem, V.PrimMonad m, Monoid elem) => Int -> m (FenMVector vector (V.PrimState m) elem)
newFen n = do
  vec <- V.replicate (n+1) mempty
  pure (FenMVector n vec)
{-# INLINABLE newFen #-}

newAccumFen :: (V.MVector vector elem, V.PrimMonad m, CommutativeMonoid elem, Foldable t) => Int -> t (Int, elem) -> m (FenMVector vector (V.PrimState m) elem)
newAccumFen n xs = do
  vec <- V.replicate (n + 1) mempty
  forM_ xs $ \(i, e) -> do
    unsafeModify' vec (<> e) i
  forM_ [1..n] $ \i -> do
    let j = i + lsb i
    when (j <= n) $ do
      e <- V.unsafeRead vec i
      unsafeModify' vec (e <>) j
  pure (FenMVector n vec)
{-# INLINABLE newAccumFen #-}


newListFen :: (V.MVector vector elem, V.PrimMonad m, CommutativeMonoid elem) => Int -> [elem] -> m (FenMVector vector (V.PrimState m) elem)
newListFen n xs = newAccumFen n $ zip [1..n] xs
{-# INLINABLE newListFen #-}

getSizeFen :: FenMVector vector s elem -> Int
getSizeFen (FenMVector n _) = n
{-# INLINE getSizeFen #-}

addFen :: (V.PrimMonad m, V.MVector vector elem, Commutative elem) => FenMVector vector (V.PrimState m) elem -> Int -> elem -> m ()
addFen (FenMVector n vec) r a = go r where
  -- 1 <= r <= n
  go i = when (i <= n) $ do
    unsafeModify' vec (a <>) i
    go (i + lsb i)
{-# INLINABLE addFen #-}

sumPrefixFen :: (Monoid elem, V.PrimMonad m, V.MVector vector elem) => FenMVector vector (V.PrimState m) elem -> Int -> m elem
sumPrefixFen (FenMVector n vec) = go mempty where
  -- prefix (0, r], for 1 <= r <= n
  go !s i
    | i <= 0 = pure s
    | otherwise = do
      x <- V.unsafeRead vec i
      go (x <> s) (i - lsb i)
{-# INLINABLE sumPrefixFen #-}

-- only applicable when partial sums are sorted
lowerBoundFen :: (Monoid elem, V.PrimMonad m, V.MVector vector elem, Ord elem) => FenMVector vector (V.PrimState m) elem -> elem -> m Int
lowerBoundFen (FenMVector n vec) query = go root (n + 1) mempty where
  root = bit (finiteBitSize n - countLeadingZeros n - 1)
  go node
    | odd node = leaf node
    | otherwise = nonleaf node
  leaf node fallback prepend 
    | node > n = pure fallback
    | otherwise = do
    nodeval <- V.unsafeRead vec node
    pure $ if prepend <> nodeval < query
      then fallback
      else node 
  nonleaf node fallback prepend 
    | node > n = go (leftOf node) fallback prepend
    | otherwise = do
    nodeval <- V.unsafeRead vec node
    if prepend <> nodeval < query
      then go (rightOf node) fallback (prepend <> nodeval)
      else go (leftOf node) node prepend
  leftOf node = node - (lsb node `unsafeShiftR` 1)
  rightOf node = node + (lsb node `unsafeShiftR` 1)
{-# INLINABLE lowerBoundFen #-}
