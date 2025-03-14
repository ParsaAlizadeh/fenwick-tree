{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.IO
import Data.Array.MArray
import Data.Bits
import Data.Monoid
import Data.Coerce
import Data.Array.Base
import Data.Bifunctor

newtype ArrayC array rep ix elem = ArrayC (array ix rep)

instance (IArray array rep, Coercible rep elem) => IArray (ArrayC array rep) elem where
  bounds (ArrayC array) = bounds array
  {-# INLINE bounds #-}

  numElements (ArrayC array) = numElements array
  {-# INLINE numElements #-}

  unsafeArray ix elems = ArrayC $ unsafeArray ix elems' where
    elems' = map (second coerce) elems
  {-# INLINE unsafeArray #-}
  
  unsafeAt (ArrayC array) ix = coerce $ unsafeAt array ix
  {-# INLINE unsafeAt #-}

  unsafeReplace (ArrayC array) elems = ArrayC $ unsafeReplace array elems' where
    elems' = map (second coerce) elems
  {-# INLINE unsafeReplace #-}
  
  unsafeAccum f (ArrayC array) elems = ArrayC $ unsafeAccum f' array elems where
    f' r e' = coerce (f (coerce r) e')
  {-# INLINE unsafeAccum #-}
  
  unsafeAccumArray f e ix elems = ArrayC $ unsafeAccumArray f' (coerce e) ix elems where
    f' r e' = coerce (f (coerce r) e')
  {-# INLINE unsafeAccumArray #-}

instance (Monad m, MArray arr r m, Coercible r e) => MArray (ArrayC arr r) e m where
  getBounds (ArrayC arr) = getBounds arr
  {-# INLINE getBounds #-}

  getNumElements (ArrayC arr) = getNumElements arr
  {-# INLINE getNumElements #-}

  newArray ix e = ArrayC <$> newArray ix (coerce e)
  {-# INLINE newArray #-}

  newArray_ ix = ArrayC <$> newArray_ ix
  {-# INLINE newArray_ #-}

  unsafeNewArray_ ix = ArrayC <$> unsafeNewArray_ ix
  {-# INLINE unsafeNewArray_ #-}

  unsafeRead (ArrayC arr) i = coerce <$> unsafeRead arr i
  {-# INLINE unsafeRead #-}

  unsafeWrite (ArrayC arr) i e = unsafeWrite arr i (coerce e)
  {-# INLINE unsafeWrite #-}

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

data FenMArray array elem = FenMArray Int (array Int elem)

modifyArray' :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray' arr i f = do
  x <- readArray arr i
  let !x' = f x
  writeArray arr i x'

instance (Monoid elem, Monad m, MArray array elem m) => FenwickLike m (FenMArray array) elem where
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

type FenMArrayC array rep elem = FenMArray (ArrayC array rep) elem

instance Num a => Group (Sum a) where
  inverse = negate

testNewFen :: Int -> IO (FenMArrayC IOUArray Int (Sum Int))
testNewFen = newFen

main = do
  fen <- testNewFen 10
  addFen fen 3 (Sum 3)
  addFen fen 7 (Sum 70)
  addFen fen 9 (Sum 100)
  sumRangeFen fen 7 8 >>= print
  forM_ [0..10] $ \i -> do
    s <- sumPrefixFen fen i
    print (i, s)
