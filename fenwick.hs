{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.IO
import Data.Array.MArray
import Data.Bits
import Data.Monoid

class Monoid a => Group a where
  inverse :: a -> a

class (Monad m, Monoid a) => FenwickLike m f a where
  newFen :: Int -> m (f a)
  addFen :: f a -> Int -> a -> m ()
  sumFen :: f a -> Int -> m a

sumRangeFen :: (FenwickLike m f a, Group a) => f a -> Int -> Int -> m a
sumRangeFen _ l r
  | l >= r = pure mempty
sumRangeFen f l r = do
  ra <- sumFen f (r - 1)
  la <- sumFen f (l - 1)
  pure (ra <> inverse la)

data FenIOArray a = FenIOArray !Int !(IOArray Int a)

modifyArray' arr i f = do
  x <- readArray arr i
  let !x' = f x
  writeArray arr i x'

instance Monoid a => FenwickLike IO FenIOArray a where
  newFen n = do
    arr <- newArray (1, n) mempty
    pure (FenIOArray n arr)
  addFen (FenIOArray n arr) r a = do
    -- 1 <= r <= n
    let go i = when (i <= n) $ do
          modifyArray' arr i (a <>)
          go (i + (i .&. (-i)))
    go r
  sumFen (FenIOArray n arr) r = do
    let go !s i = if i <= 0 then pure s else do
          x <- readArray arr i
          go (x <> s) (i - (i .&. (-i)))
    go mempty r

instance Num a => Group (Sum a) where
  inverse = negate

testNewFen :: Group a => Int -> IO (FenIOArray a)
testNewFen = newFen

test = do
  fen <- testNewFen 10
  addFen fen 3 (Sum 10)
  addFen fen 7 (Sum 1)
  addFen fen 10 (Sum 100)
  sumRangeFen fen 7 8 >>= print
  forM_ [0..10] $ \i -> do
    s <- sumFen fen i
    print (i, s)
