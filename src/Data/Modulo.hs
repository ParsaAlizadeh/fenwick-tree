{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module Data.Modulo (
  Modulo(..),
  getMod
) where

import GHC.TypeLits
import Data.Proxy

type role Modulo nominal nominal
newtype Modulo a (m :: Nat) = UnsafeModulo a
  deriving (Eq, Ord)

getMod :: (Num a, KnownNat m) => Proxy m -> a
getMod = fromInteger . natVal

instance (KnownNat m, Show a) => Show (Modulo a m) where
  show (UnsafeModulo a) = "(" <> show a <> " mod " <> show m' <> ")" where
    m' = getMod (Proxy @m)

instance (KnownNat m, Integral a) => Num (Modulo a m) where
  (UnsafeModulo !a) + (UnsafeModulo !b)
    | a + b >= m' = UnsafeModulo (a + b - m')
    | otherwise = UnsafeModulo (a + b) where 
        m' = getMod (Proxy @m)

  (UnsafeModulo !a) * (UnsafeModulo !b) = UnsafeModulo $ (a * b) `mod` m' where
    m' = getMod (Proxy @m)

  abs = id

  signum = const 1

  fromInteger n = UnsafeModulo . fromInteger $ n `mod` m' where
    m' = getMod (Proxy @m)

  negate (UnsafeModulo !n) = UnsafeModulo $ if n == 0 then 0 else m' - n where
    m' = getMod (Proxy @m)

instance (KnownNat m, Integral a) => Real (Modulo a m) where
  toRational (UnsafeModulo a) = toRational a

instance (KnownNat m, Num a) => Bounded (Modulo a m) where
  minBound = UnsafeModulo 0
  maxBound = UnsafeModulo (m' - 1) where
    m' = getMod (Proxy @m)

instance (KnownNat m, Integral a) => Enum (Modulo a m) where
  succ (UnsafeModulo a)
    | a >= m' - 1 = error "succ (mod - 1)"
    | otherwise = UnsafeModulo (a + 1) where
        m' = getMod (Proxy @m)
  pred (UnsafeModulo 0) = error "pred 0"
  pred (UnsafeModulo a) = UnsafeModulo (a - 1)
  toEnum a
    | a >= m' = error "toEnum larger than modulo"
    | otherwise = UnsafeModulo (fromIntegral a) where
        m' = getMod (Proxy @m)
  fromEnum (UnsafeModulo a) = fromEnum a

binPow :: Num a => a -> Int -> a
binPow _ 0 = 1
binPow a 1 = a
binPow !a b
  | even b = x * x
  | otherwise = a * x * x where
    !x = binPow (a * a) (b `div` 2)

instance (KnownNat m, Integral a) => Integral (Modulo a m) where
  quotRem _ (UnsafeModulo 0) = error "divide by zero"
  quotRem a b = (a * (b `binPow` (m' - 2)), 0) where
    m' = getMod (Proxy @m)
  divMod = quotRem
  toInteger (UnsafeModulo a) = toInteger a
