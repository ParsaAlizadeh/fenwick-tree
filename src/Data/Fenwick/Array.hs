{-# LANGUAGE BangPatterns #-}

module Data.Fenwick.Array where

import Data.Array.MArray
import Control.Monad
import Data.Bits
import Data.Group

data FenMArray array elem = FenMArray Int (array Int elem)

modifyArray' :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray' arr i f = do
  x <- readArray arr i
  let !x' = f x
  writeArray arr i x'

newFen :: (MArray array elem m, Monoid elem) => Int -> m (FenMArray array elem)
newFen n = do
  arr <- newArray (1, n) mempty
  pure (FenMArray n arr)

addFen :: (MArray array elem f, Semigroup elem) => FenMArray array elem -> Int -> elem -> f ()
addFen (FenMArray n arr) r a = do
  -- 0 <= r < n - 1
  let go i = when (i <= n) $ do
        modifyArray' arr i (a <>)
        go (i + (i .&. (-i)))
  go (r + 1)

sumPrefixFen :: (MArray array elem f, Monoid elem) => FenMArray array elem -> Int -> f elem
sumPrefixFen (FenMArray n arr) r = do
  let go !s i = if i <= 0 then pure s else do
        x <- readArray arr i
        go (x <> s) (i - (i .&. (-i)))
  go mempty r

sumRangeFen :: (MArray array elem m, Group elem) => FenMArray array elem -> Int -> Int -> m elem
sumRangeFen _ l r
  | l >= r = pure mempty
sumRangeFen f l r = do
  ra <- sumPrefixFen f r
  la <- sumPrefixFen f l
  pure (ra <> inverse la)
