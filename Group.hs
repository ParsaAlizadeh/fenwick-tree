{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables, RoleAnnotations, TypeApplications, BangPatterns #-}

module Group where

import Data.Monoid
import GHC.TypeLits
import Data.Proxy
import Data.Coerce

class Monoid a => Group a where
  inverse :: a -> a

instance Num a => Group (Sum a) where
  inverse = negate

type role Modulo nominal nominal
newtype Modulo a (m :: Nat) = Modulo a
    deriving (Show, Eq, Ord)

getMod :: (Integral a, KnownNat m) => proxy m -> a
getMod = fromInteger . natVal

instance (KnownNat m, Integral a) => Num (Modulo a m) where
    x@(Modulo !a) + (Modulo !b) = Modulo $ 
        if a + b >= m' then a + b - m' else a + b where
        m' = getMod x
    
    x@(Modulo !a) * (Modulo !b) = Modulo $ (a * b) `mod` m' where
        m' = getMod x
    
    abs = id

    signum = const 1

    fromInteger n = Modulo . fromInteger $ n `mod` m' where
        m' = getMod (Proxy :: Proxy m)
    
    negate x@(Modulo !n) = Modulo $ if n == 0 then 0 else m' - n where
        m' = getMod x

binPow :: Num a => a -> Int -> a
binPow _ 0 = 1
binPow a 1 = a
binPow !a b
    | even b = x * x
    | otherwise = a * x * x where
        !x = binPow (a * a) (b `div` 2)

instance (Integral a, KnownNat m) => Group (Product (Modulo a m)) where
    inverse = coerce go where
        go (a :: Modulo a m) = a `binPow` (getMod a - 2)