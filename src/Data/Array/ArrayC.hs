{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Module       : Data.Array.ArrayC
-- 
-- This modulo provides 'ArrayC', which given a existing 'MArray' implementation and a
-- representation type, it stores elements by coercing the element type into representation type.
-- Using this, we can have elements with algebraic structures (like 'Data.Monoid.Monoid' used in
-- this package) but stored in efficient unboxed arrays.
module Data.Array.ArrayC ( ArrayC ) where

import Data.Array.MArray
import Data.Array.Base hiding (array, elems)
import Data.Coerce

-- | Given an array type (e.g. 'Data.Array.IO.IOUArray') and a representation type (e.g. 'Int'), the
-- type constructor @'ArrayC' 'Data.Array.IO.IOUArray' 'Int'@ is a valid array type, that can store
-- elements that are coercible to 'Int' (the representation type). The same type can be used for
-- immutable and mutable arrays. Use functions from 'Data.Array.IArray' and 'Data.Array.MArray' to
-- create and modify the array.
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
