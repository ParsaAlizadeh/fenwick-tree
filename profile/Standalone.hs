{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Array.IO
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Builder
import Data.List
import Data.Maybe
import Control.Monad
import Data.Array.MArray
import Data.Bits
import Data.Array.Base
import Data.Monoid
import GHC.TypeLits
import Data.Proxy
import Data.Coerce
import System.IO
import Data.IORef

class Monoid a => Group a where
  inverse :: a -> a

instance Num a => Group (Sum a) where
  inverse = negate
  {-# INLINE inverse #-}

newtype ArrayC array rep ix elem = ArrayC (array ix rep)

instance (IArray array rep, Coercible rep elem) => IArray (ArrayC array rep) elem where
  bounds (ArrayC array) = bounds array
  {-# INLINE bounds #-}

  numElements (ArrayC array) = numElements array
  {-# INLINE numElements #-}

  unsafeArray ix elems = ArrayC $ unsafeArray ix (coerce elems)
  {-# INLINE unsafeArray #-}

  unsafeAt (ArrayC array) ix = coerce $ unsafeAt array ix
  {-# INLINE unsafeAt #-}

  unsafeReplace (ArrayC array) elems = ArrayC $ unsafeReplace array (coerce elems)
  {-# INLINE unsafeReplace #-}

  unsafeAccum f (ArrayC array) elems = ArrayC $ unsafeAccum (coerce f) array elems
  {-# INLINE unsafeAccum #-}

  unsafeAccumArray f e ix elems = ArrayC $ unsafeAccumArray (coerce f) (coerce e) ix elems
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

class (Monad m, Monoid a) => FenwickLike m f a where
  newFen :: Int -> m (f a)
  addFen :: f a -> Int -> a -> m ()
  sumPrefixFen :: f a -> Int -> m a

sumRangeFen :: (FenwickLike m f a, Group a) => f a -> Int -> Int -> m a
sumRangeFen _ l r
  | l >= r = pure mempty
sumRangeFen f l r = do
  ra <- sumPrefixFen f (r - 1)
  la <- sumPrefixFen f (l - 1)
  pure (ra <> inverse la)
{-# INLINABLE sumRangeFen #-}

data FenMArray array elem = FenMArray Int (array Int elem)

modifyArray' :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray' arr i f = do
  x <- readArray arr i
  let !x' = f x
  writeArray arr i x'
{-# INLINE modifyArray' #-}

instance (Monoid elem, Monad m, MArray array elem m) => FenwickLike m (FenMArray array) elem where
  newFen n = do
    arr <- newArray (1, n) mempty
    pure (FenMArray n arr)

  addFen (FenMArray n arr) r a = do
    -- 1 <= r <= n
    let go i = when (i <= n) $ do
          modifyArray' arr i (a <>)
          go (i + (i .&. (-i)))
    go r

  sumPrefixFen (FenMArray n arr) r = do
    let go !s i = if i <= 0 then pure s else do
          x <- readArray arr i
          go (x <> s) (i - (i .&. (-i)))
    go mempty r

type FenMArrayC array rep elem = FenMArray (ArrayC array rep) elem

newFenSum :: Int -> IO (FenMArrayC IOUArray Int (Sum Int))
newFenSum = newFen

readInts :: IO [Int]
readInts = map (fst . fromJust . C.readInt) . C.words <$> C.getLine

printInts :: [Int] -> IO ()
printInts xs = hPutBuilder stdout (go <> char7 '\n') where
  go = mconcat . intersperse (char7 ' ') . map intDec $ xs

printInt :: Int -> IO ()
printInt x = hPutBuilder stdout (intDec x <> charUtf8 '\n')

main = do
  hSetBuffering stdin (BlockBuffering Nothing)
  hSetBuffering stdout (BlockBuffering Nothing)
  [n] <- readInts
  fen <- newFenSum n
  let go = do
        typ : query <- readInts
        case typ of
          0 -> do
            let [p, x] = query
            addFen fen p (Sum x)
          1 -> do
            let [r] = query
            sumPrefixFen fen r >>= printInt . getSum
          2 -> do
            let [l, r] = query
            sumRangeFen fen l r >>= printInt . getSum
          -1 -> pure ()
        when (typ /= -1) go
  go
