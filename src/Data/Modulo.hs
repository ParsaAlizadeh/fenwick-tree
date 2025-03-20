{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Data.Modulo (
  Modulo(..),
  getMod
) where

import GHC.TypeLits
import Data.Proxy
import Foreign.Storable
import Data.Semigroup.Cancellative
import Numeric.Product.Commutative

type role Modulo nominal nominal
newtype Modulo a (m :: Nat) = Modulo a
  deriving (Eq, Ord, Storable)

getMod :: (Num a, KnownNat m) => Proxy m -> a
getMod = fromInteger . natVal

instance (KnownNat m, Show a) => Show (Modulo a m) where
  show (Modulo a) = "(" <> show a <> " mod " <> show m' <> ")" where
    m' = getMod (Proxy @m)

instance (KnownNat m, Integral a) => Num (Modulo a m) where
  (Modulo !a) + (Modulo !b)
    | a + b >= m' = Modulo (a + b - m')
    | otherwise = Modulo (a + b) where 
        m' = getMod (Proxy @m)

  (Modulo !a) * (Modulo !b) = Modulo $ (a * b) `mod` m' where
    m' = getMod (Proxy @m)

  abs = id

  signum = const 1

  fromInteger n = Modulo . fromInteger $ n `mod` m' where
    m' = getMod (Proxy @m)

  negate (Modulo !n) = Modulo $ if n == 0 then 0 else m' - n where
    m' = getMod (Proxy @m)

instance (KnownNat m, Integral a) => Real (Modulo a m) where
  toRational (Modulo a) = toRational a

instance (KnownNat m, Num a) => Bounded (Modulo a m) where
  minBound = Modulo 0
  maxBound = Modulo (m' - 1) where
    m' = getMod (Proxy @m)

instance (KnownNat m, Integral a) => Enum (Modulo a m) where
  succ (Modulo a)
    | a >= m' - 1 = error "succ (mod - 1)"
    | otherwise = Modulo (a + 1) where
        m' = getMod (Proxy @m)
  pred (Modulo 0) = error "pred 0"
  pred (Modulo a) = Modulo (a - 1)
  toEnum a
    | a >= m' = error "toEnum larger than modulo"
    | otherwise = Modulo (fromIntegral a) where
        m' = getMod (Proxy @m)
  fromEnum (Modulo a) = fromEnum a

binPow :: Num a => a -> Int -> a
binPow _ 0 = 1
binPow a 1 = a
binPow !a b
  | even b = x * x
  | otherwise = a * x * x where
    !x = binPow (a * a) (b `div` 2)

instance (KnownNat m, Integral a) => Integral (Modulo a m) where
  quotRem _ (Modulo 0) = error "divide by zero"
  quotRem a b = (a * (b `binPow` (m' - 2)), 0) where
    m' = getMod (Proxy @m)
  divMod = quotRem
  toInteger (Modulo a) = toInteger a

instance (KnownNat m, Integral a) => SumCancellative (Modulo a m)
instance (KnownNat m, Integral a) => CommutativeProduct (Modulo a m)
