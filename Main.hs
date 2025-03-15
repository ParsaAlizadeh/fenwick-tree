{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables, RoleAnnotations, TypeApplications #-}

module Main where

import Fenwick
import Data.Monoid
import Control.Monad.ST
import Data.Array.IO
import Control.Monad
import Text.Printf
import GHC.TypeLits
import Data.Proxy
import Data.Coerce

instance Num a => Group (Sum a) where
  inverse = negate

newFenSum :: Int -> IO (FenMArrayC IOUArray Int (Sum Int))
newFenSum = newFen

type role Modulo nominal
newtype Modulo (m :: Nat) = Modulo Int
    deriving (Show, Eq, Ord)

getMod :: KnownNat m => proxy m -> Int
getMod = fromInteger . natVal

instance KnownNat m => Num (Modulo m) where
    x@(Modulo a) + (Modulo b) = Modulo $ 
        if a + b >= m' then a + b - m' else a + b where
        m' = getMod x
    
    x@(Modulo a) * (Modulo b) = Modulo $ (a * b) `mod` m' where
        m' = getMod x
    
    abs = id

    signum = const 1

    fromInteger n = Modulo . fromInteger $ n `mod` m' where
        m' = natVal (Proxy :: Proxy m)
    
    negate x@(Modulo n) = Modulo $ m' - n where
        m' = getMod x

newtype SumMod m = SumMod (Modulo m)
    deriving (Show, Eq, Ord, Num)

instance KnownNat m => Semigroup (SumMod m) where
    (<>) = (+)

instance KnownNat m => Monoid (SumMod m) where
    mempty = 0

newtype ProductMod m = ProductMod (Modulo m)
    deriving (Show, Eq, Ord, Num)

instance KnownNat m => Semigroup (ProductMod m) where
    (<>) = (*)

instance KnownNat m => Monoid (ProductMod m) where
    mempty = 1

type T = ProductMod 1000000007
newFenSumMod :: Int -> IO (FenMArrayC IOUArray Int T)
newFenSumMod = newFen

binPow :: Num a => a -> Int -> a
binPow a 0 = 1
binPow a 1 = a
binPow a b
    | even b = x * x
    | otherwise = a * x * x where
        x = binPow a (b `div` 2)

instance KnownNat m => Group (SumMod m) where
    inverse = negate

instance KnownNat m => Group (ProductMod m) where
    inverse a = a `binPow` (m' - 2) where
        m' = getMod a

main = do
    fen <- newFenSumMod 10
    addFen fen 2 (-1)
    addFen fen 6 5
    n :: Int <- coerce <$> sumRangeFen fen 2 7
    print n
    forM_ [0..10] $ \r -> do
        n :: Int <- coerce <$> sumPrefixFen fen r
        printf "%3d %3d\n" r n
