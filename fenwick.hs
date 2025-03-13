{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.IO
import Data.Array.MArray
import Data.Bits
import Data.Monoid
import Data.Coerce

class Monoid a => Group a where
  inverse :: a -> a

class (Monad m, Monoid a) => FenwickLike m f a where
  newFen :: Int -> m (f a)
  addFen :: f a -> Int -> a -> m ()
  sumPrefixFen :: f a -> Int -> m a

sumRangeFen :: (FenwickLike m f a, Group a) => f a -> Int -> Int -> m a
sumRangeFen _ l r
  | l >= r = pure mempty
sumRangeFen f l r = do
  ra <- sumPrefixFen f r
  la <- sumPrefixFen f l
  pure (ra <> inverse la)

data FenMArray arr a = FenMArray !Int !(arr Int a)

modifyArray' arr i f = do
  x <- readArray arr i
  let !x' = f x
  writeArray arr i x'

instance (Monoid a, Monad m, MArray arr a m) => FenwickLike m (FenMArray arr) a where
  newFen n = do
    arr <- newArray (1, n) mempty
    pure (FenMArray n arr)
  addFen (FenMArray n arr) r a = do
    -- 0 <= r < n - 1
    let go i = when (i <= n) $ do
          modifyArray' arr i (a <>)
          go (i + (i .&. (-i)))
    go (r + 1)
  sumPrefixFen (FenMArray n arr) r = do
    let go !s i = if i <= 0 then pure s else do
          x <- readArray arr i
          go (x <> s) (i - (i .&. (-i)))
    go mempty r

data FenMArrayC arr r a = FenMArrayC !Int !(arr Int r)

instance (Monoid a, Monad m, MArray arr r m, Coercible r a)
  => FenwickLike m (FenMArrayC arr r) a where
  newFen n = do
    arr <- newArray (1, n) (coerce (mempty :: a) :: r)
    pure (FenMArrayC n arr)
  addFen (FenMArrayC n arr) r a = do
    -- 0 <= r < n - 1
    let go i = when (i <= n) $ do
          modifyArray' arr i (coerce . (a <>) . coerce)
          go (i + (i .&. (-i)))
    go (r + 1)
  sumPrefixFen (FenMArrayC n arr) r = do
    let go !s i = if i <= 0 then pure s else do
          x <- coerce <$> readArray arr i
          go (x <> s) (i - (i .&. (-i)))
    go mempty r

instance Num a => Group (Sum a) where
  inverse = negate

testNewFen :: Int -> IO (FenMArrayC IOUArray Int (Sum Int))
testNewFen = newFen

test = do
  fen <- testNewFen 10
  addFen fen 3 (Sum 3)
  addFen fen 7 (Sum 70)
  addFen fen 9 (Sum 100)
  sumRangeFen fen 7 8 >>= print
  forM_ [0..10] $ \i -> do
    s <- sumPrefixFen fen i
    print (i, s)
