{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Array.ArrayC where

import Data.Array.MArray
import Data.Array.Base
import Data.Coerce

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
