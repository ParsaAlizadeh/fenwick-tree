{-# LANGUAGE BangPatterns #-}

-- | Module       : Data.Fenwick.Array
-- 
-- This modulo provides mutable [Fenwick Trees](https://en.wikipedia.org/wiki/Fenwick_tree), using
-- arrays as the underlying data structure. The algebraic structure is given by 'Semigroup' and
-- 'Monoid' instances. Some of the functions require the structure to be
-- 'Data.Semigroup.Commutative.Commutative'. See @monoid-subclasses@ and @commutative-semigroups@.
-- If you want mark @Sum@ or @Product@ as @Commutative@, see @SumCommutative@ and
-- @ProductCommutative@ in the mentioned packages.

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

-- | Fenwick tree datatype. @array@ must be a type such that @array 'Int' elem@ refers to a valid
-- mutable array. Fenwick tree is assumed to be data structure over a 1-based array of size @n@.
data FenMArray array elem = FenMArray !Int !(array Int elem)

-- | least significant bit
lsb :: Int -> Int
lsb node = node .&. (-node)
{-# INLINE lsb #-}

modifyArray' :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray' arr i f = do
  x <- readArray arr i
  let !x' = f x
  writeArray arr i x'
{-# INLINABLE modifyArray' #-}

unsafeModifyArray' :: (MArray a e m, Ix i) => a i e -> Int -> (e -> e) -> m ()
unsafeModifyArray' arr i f = do
  x <- unsafeRead arr i
  let !x' = f x
  unsafeWrite arr i x'
{-# INLINABLE unsafeModifyArray' #-}

-- | Creates a Fenwick tree of size @n@ over 1-based array. All elements of the array are initially
-- @'mempty'@. \( O(n) \)
--
-- Using type application extension, you can specify the array and element type as the first and
-- second type argument.
-- 
-- @ 
-- fen <- 'newFen' \@('Data.Array.ArrayC.ArrayC' 'Data.Array.IO.IOUArray' 'Int') \@('Data.Monoid.Sum' 'Int') 
-- @
newFen :: (MArray array elem m, Monoid elem) => Int -> m (FenMArray array elem)
newFen n = do
  arr <- newArray (0, n) mempty
  pure (FenMArray n arr)
{-# INLINABLE newFen #-}

-- | @newAccumFen n xs@ creates a Fenwick tree of size @n@ over 1-based array. Every item of @xs@ is
-- pair of an index and an element. The elements of the array are initialized by @'<>'@ing elements
-- for each index. This functions is faster than creating empty array using 'newFen' and adding
-- elements individually using 'addFen'. \( O(n + m) \) where \( m \) is @'Data.Foldable.length' xs@
newAccumFen :: (MArray array elem m, CommutativeMonoid elem, Foldable t) => Int -> t (Int, elem) -> m (FenMArray array elem)
newAccumFen n xs = do
  arr <- newArray (0, n) mempty
  forM_ xs $ \(i, e) -> do
    modifyArray' arr i (<> e)
  forM_ [1..n] $ \i -> do
    let j = i + lsb i
    when (j <= n) $ do
      e <- readArray arr i
      unsafeModifyArray' arr j (e <>)
  pure (FenMArray n arr)
{-# INLINABLE newAccumFen #-}

-- | Creates a Fenwick tree and initialize the array based on the elements of the list. \( O(n) \)
newListFen :: (MArray array elem m, CommutativeMonoid elem) => Int -> [elem] -> m (FenMArray array elem)
newListFen n xs = newAccumFen n $ zip [1..n] xs
{-# INLINABLE newListFen #-}

-- | Get the size of underlying array. \( O(1) \)
getSizeFen :: FenMArray array elem -> Int
getSizeFen (FenMArray n _) = n
{-# INLINE getSizeFen #-}

-- | Add a value to a cell of the array. The index must be in the range @[1, n]@. \( O(\log n) \)
addFen :: (MArray array elem m, Commutative elem) => FenMArray array elem -> Int -> elem -> m ()
addFen (FenMArray n arr) r a = go (check r) where
  check i
    | i < 1 || i > n = error "index out of range"
    | otherwise = i
  go i = when (i <= n) $ do
    unsafeModifyArray' arr i (a <>)
    go (i + lsb i)
{-# INLINABLE addFen #-}

-- | Given index @r@, get the prefix sum of elements of the array in the range @[1, r]@. It accepts
-- values of @r@ out of the range of indices, assuming that every element out of the range of array
-- is 'mempty'. \( O(\log n) \)
sumPrefixFen :: (MArray array elem m, Monoid elem) => FenMArray array elem -> Int -> m elem
sumPrefixFen (FenMArray n arr) = go mempty . min n where
  -- prefix (0, r], for 1 <= r <= n
  go !s i
    | i <= 0 = pure s
    | otherwise = do
      x <- unsafeRead arr i
      go (x <> s) (i - lsb i)
{-# INLINABLE sumPrefixFen #-}

-- | Given a prefix sum @q@, find the least index @r@ such that the prefix sum of @[1, r]@ is at
-- least @q@. Only applicable when partial sums are ordered. If the sum of the whole array is
-- less than @query@, it returns @n+1@. This functions is faster than binary search over
-- 'sumPrefixFen'. \( O(\log n) \) 
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
