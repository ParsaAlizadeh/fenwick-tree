{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Module       : Data.Modulo
-- 
-- This module provides Modulo type, that keeps track of the modulo @m@ in the type and implements
-- arithmetic operations modulo @m@. The implementation of @Integral Modulo@ (i.e. division) is only
-- valid when @m@ is prime.
module Data.Modulo (
  Modulo(..),
  getMod
) where

import GHC.TypeLits
import Data.Proxy
import Foreign.Storable
import Data.Semigroup.Cancellative
import Numeric.Product.Commutative

-- | Using constructor of @Modulo@ is *unsafe*! This is exported only to be used by @Coercible@
-- instance in 'Data.Array.ArrayC'. If you want to ensure safe usage of this type, hide the constructor.
--
-- > import qualified Data.Modulo hiding (Modulo(Modulo))
type role Modulo nominal nominal
newtype Modulo a (m :: Nat) = Modulo a
  deriving (Eq, Ord, Storable)

-- | Given a value holding modulo @m@ (like 'Modulo' or @Proxy@), it returns the modulo.
getMod :: (Num a, KnownNat m) => proxy m -> a
getMod = fromInteger . natVal

instance (KnownNat m, Show a) => Show (Modulo a m) where
  show (Modulo a) = "(" <> show a <> " mod " <> show m' <> ")" where
    m' :: Int = getMod (Proxy @m)

instance (KnownNat m, Integral a) => Num (Modulo a m) where
  (Modulo !a) + (Modulo !b)
    | a + b >= m' = Modulo (a + b - m')
    | otherwise = Modulo (a + b) where 
        m' = getMod (Proxy @m)
  {-# INLINE (+) #-}

  (Modulo !a) * (Modulo !b) = Modulo $ (a * b) `mod` m' where
    m' = getMod (Proxy @m)
  {-# INLINE (*) #-}

  abs = id
  {-# INLINE abs #-}

  signum = const 1
  {-# INLINE signum #-}

  fromInteger n = Modulo . fromInteger $ n `mod` m' where
    m' = getMod (Proxy @m)
  {-# INLINE fromInteger #-}

  negate (Modulo !n) = Modulo $ if n == 0 then 0 else m' - n where
    m' = getMod (Proxy @m)
  {-# INLINE negate #-}

instance (KnownNat m, Integral a) => Real (Modulo a m) where
  toRational (Modulo a) = toRational a
  {-# INLINE toRational #-}

instance (KnownNat m, Num a) => Bounded (Modulo a m) where
  minBound = Modulo 0
  {-# INLINE minBound #-}
  maxBound = Modulo (m' - 1) where
    m' = getMod (Proxy @m)
  {-# INLINE maxBound #-}

instance (KnownNat m, Integral a) => Enum (Modulo a m) where
  succ (Modulo a)
    | a >= m' - 1 = error "succ (mod - 1)"
    | otherwise = Modulo (a + 1) where
        m' = getMod (Proxy @m)
  {-# INLINE succ #-}
  pred (Modulo 0) = error "pred 0"
  pred (Modulo a) = Modulo (a - 1)
  {-# INLINE pred #-}
  toEnum a
    | a >= m' = error "toEnum larger than modulo"
    | otherwise = Modulo (fromIntegral a) where
        m' = getMod (Proxy @m)
  {-# INLINE toEnum #-}
  fromEnum (Modulo a) = fromEnum a
  {-# INLINE fromEnum #-}

binPow :: Num a => a -> Int -> a
binPow _ 0 = 1
binPow a 1 = a
binPow !a b
  | even b = x * x
  | otherwise = a * x * x where
    !x = binPow (a * a) (b `div` 2)
{-# INLINE binPow #-}

instance (KnownNat m, Integral a) => Integral (Modulo a m) where
  quotRem _ (Modulo 0) = error "divide by zero"
  quotRem a b = (a * (b `binPow` (m' - 2)), 0) where
    m' = getMod (Proxy @m)
  {-# INLINE quotRem #-}
  divMod = quotRem
  {-# INLINE divMod #-}
  toInteger (Modulo a) = toInteger a
  {-# INLINE toInteger #-}

instance (KnownNat m, Integral a) => SumCancellative (Modulo a m)
instance (KnownNat m, Integral a) => CommutativeProduct (Modulo a m)
