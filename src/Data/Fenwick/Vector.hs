{-# LANGUAGE BangPatterns #-}

-- | Module       : Data.Fenwick.Vector
-- 
-- This modulo provides mutable [https://en.wikipedia.org/wiki/Fenwick_tree](Fenwick Trees), using
-- vectors as the underlying data structure. The algebraic structure is given by 'Semigroup' and
-- 'Monoid' instances. Some of the functions require the structure to be 'Commutative'. See
-- @monoid-subclasses@ and @commutative-semigroups@. If you want mark @Sum@ or @Product@ as
-- @Commutative@, see @SumCommutative@ and @ProductCommutative@ in the mentioned packages.
--
-- I was hoping to merge this module with 'Data.Fenwick.Array' under a common typeclass, although I
-- couldn't find any way through this. I may change this later, if I find the right abstractions. 

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

-- | Fenwick tree datatype. @vector@ must be a valid mutable vector type. Fenwick tree is assumed to
-- be data structure over a 1-based array of size @n@.
data FenMVector vector s elem = FenMVector !Int !(vector s elem)

-- | least significant bit
lsb :: Int -> Int
lsb node = node .&. (-node)
{-# INLINE lsb #-}

modify' :: (V.PrimMonad m, V.MVector v a) => v (V.PrimState m) a -> (a -> a) -> Int -> m ()
modify' vec f = V.modify vec (f $!)
{-# INLINABLE modify' #-}

unsafeModify' :: (V.PrimMonad m, V.MVector v a) => v (V.PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModify' vec f = V.unsafeModify vec (f $!)
{-# INLINABLE unsafeModify' #-}

-- | Creates a Fenwick tree of size @n@ over 1-based array. All elements of the array are initially
-- @'mempty'@. \( O(n) \)
--
-- Using type application extension, you can specify the vector and element type as the first and
-- second type argument.
newFen :: (V.MVector vector elem, V.PrimMonad m, Monoid elem) => Int -> m (FenMVector vector (V.PrimState m) elem)
newFen n = do
  vec <- V.replicate (n+1) mempty
  pure (FenMVector n vec)
{-# INLINABLE newFen #-}

-- | @newAccumFen n xs@ creates a Fenwick tree of size @n@ over 1-based array. Every item of @xs@ is
-- pair of an index and an element. The elements of the array are initialized by @'<>'@ing elements
-- for each index. This functions is faster than creating empty array using 'newFen' and adding
-- elements individually using 'addFen'. \( O(n + m) \) where \( m \) is @'Data.Foldable.length' xs@
newAccumFen :: (V.MVector vector elem, V.PrimMonad m, CommutativeMonoid elem, Foldable t) => Int -> t (Int, elem) -> m (FenMVector vector (V.PrimState m) elem)
newAccumFen n xs = do
  vec <- V.replicate (n + 1) mempty
  forM_ xs $ \(i, e) -> do
    modify' vec (<> e) i
  forM_ [1..n] $ \i -> do
    let j = i + lsb i
    when (j <= n) $ do
      e <- V.unsafeRead vec i
      unsafeModify' vec (e <>) j
  pure (FenMVector n vec)
{-# INLINABLE newAccumFen #-}

-- | Creates a Fenwick tree and initialize the array based on the elements of the list. \( O(n) \)
newListFen :: (V.MVector vector elem, V.PrimMonad m, CommutativeMonoid elem) => Int -> [elem] -> m (FenMVector vector (V.PrimState m) elem)
newListFen n xs = newAccumFen n $ zip [1..n] xs
{-# INLINABLE newListFen #-}

-- | Get the size of underlying array. \( O(1) \)
getSizeFen :: FenMVector vector s elem -> Int
getSizeFen (FenMVector n _) = n
{-# INLINE getSizeFen #-}

-- | Add a value to a cell of the array. The index must be in the range @[1, n]@. \( O(\log n) \)
addFen :: (V.PrimMonad m, V.MVector vector elem, Commutative elem) => FenMVector vector (V.PrimState m) elem -> Int -> elem -> m ()
addFen (FenMVector n vec) r a = go (check r) where
  check i
    | i < 1 || i > n = error "index out of range"
    | otherwise = i
  go i = when (i <= n) $ do
    unsafeModify' vec (a <>) i
    go (i + lsb i)
{-# INLINABLE addFen #-}

-- | Given index @r@, get the prefix sum of elements of the array in the range @[1, r]@. It accepts
-- values of @r@ out of the range of indices, assuming that every element out of the range of array
-- is 'mempty'. \( O(\log n) \)
sumPrefixFen :: (Monoid elem, V.PrimMonad m, V.MVector vector elem) => FenMVector vector (V.PrimState m) elem -> Int -> m elem
sumPrefixFen (FenMVector n vec) = go mempty . min n where
  -- prefix (0, r], for 1 <= r <= n
  go !s i
    | i <= 0 = pure s
    | otherwise = do
      x <- V.unsafeRead vec i
      go (x <> s) (i - lsb i)
{-# INLINABLE sumPrefixFen #-}

-- | Given a prefix sum @q@, find the least index @r@ such that the prefix sum of @[1, r]@ is at
-- least @q@. Only applicable when partial sums are ordered. If the sum of the whole array is
-- less than @query@, it returns @n+1@. This functions is faster than binary search over
-- 'sumPrefixFen'. \( O(\log n) \) 
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
