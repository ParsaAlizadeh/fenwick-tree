{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Vector.VectorC (
  VectorC, MVectorC
) where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Data.Coerce

newtype MVectorC vector rep s elem = MVectorC (vector s rep)
newtype VectorC vector rep elem = VectorC (vector rep)

type instance VG.Mutable (VectorC vector rep) = MVectorC (VG.Mutable vector) rep

instance (Coercible elem rep, VGM.MVector vector rep) => VGM.MVector (MVectorC vector rep) elem where
  -- basicLength = coerce $ VGM.basicLength @vector @rep
  basicLength (MVectorC vec) = VGM.basicLength vec
  {-# INLINE basicLength #-}
  -- basicUnsafeSlice = coerce $ VGM.basicUnsafeSlice @VU.MVector
  basicUnsafeSlice i n (MVectorC vec) = MVectorC (VGM.basicUnsafeSlice i n vec)
  {-# INLINE basicUnsafeSlice #-}
  -- basicOverlaps = coerce $ VGM.basicOverlaps @VU.MVector
  basicOverlaps (MVectorC vec1) (MVectorC vec2) = VGM.basicOverlaps vec1 vec2
  {-# INLINE basicOverlaps #-}
  -- basicUnsafeNew = coerce $ VGM.basicUnsafeNew @VU.MVector
  basicUnsafeNew n = MVectorC <$> VGM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  -- basicInitialize = coerce $ VGM.basicInitialize @VU.MVector
  basicInitialize (MVectorC vec) = VGM.basicInitialize vec
  {-# INLINE basicInitialize #-}
  -- basicUnsafeReplicate = coerce $ VGM.basicUnsafeReplicate @VU.MVector
  basicUnsafeReplicate n a = MVectorC <$> VGM.basicUnsafeReplicate n (coerce a)
  {-# INLINE basicUnsafeReplicate #-}
  -- basicUnsafeRead = coerce $ VGM.basicUnsafeRead @VU.MVector
  basicUnsafeRead (MVectorC vec) i = coerce <$> VGM.basicUnsafeRead vec i
  {-# INLINE basicUnsafeRead #-}
  -- basicUnsafeWrite = coerce $ VGM.basicUnsafeWrite @VU.MVector
  basicUnsafeWrite (MVectorC vec) i a = VGM.basicUnsafeWrite vec i (coerce a)
  {-# INLINE basicUnsafeWrite #-}
  -- basicClear = coerce $ VGM.basicClear @VU.MVector
  basicClear (MVectorC vec) = VGM.basicClear vec
  {-# INLINE basicClear #-}
  -- basicSet = coerce $ VGM.basicSet @VU.MVector
  basicSet (MVectorC vec) a = VGM.basicSet vec (coerce a)
  {-# INLINE basicSet #-}
  -- basicUnsafeCopy = coerce $ VGM.basicUnsafeCopy @VU.MVector
  basicUnsafeCopy (MVectorC vec1) (MVectorC vec2) = VGM.basicUnsafeCopy vec1 vec2
  {-# INLINE basicUnsafeCopy #-}
  -- basicUnsafeMove = coerce $ VGM.basicUnsafeMove @VU.MVector
  basicUnsafeMove (MVectorC vec1) (MVectorC vec2) = VGM.basicUnsafeMove vec1 vec2
  {-# INLINE basicUnsafeMove #-}
  -- basicUnsafeGrow = coerce $ VGM.basicUnsafeGrow @VU.MVector
  basicUnsafeGrow (MVectorC vec) n = MVectorC <$> VGM.basicUnsafeGrow vec n
  {-# INLINE basicUnsafeGrow #-}

instance (Coercible elem rep, VG.Vector vector rep) => VG.Vector (VectorC vector rep) elem where
  basicUnsafeFreeze = coerce $ VG.basicUnsafeFreeze @vector @rep
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw = coerce $ VG.basicUnsafeThaw @vector @rep
  {-# INLINE basicUnsafeThaw #-}
  basicLength = coerce $ VG.basicLength @vector @rep
  {-# INLINE basicLength #-}
  basicUnsafeSlice = coerce $ VG.basicUnsafeSlice @vector @rep
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM = coerce $ VG.basicUnsafeIndexM @vector @rep
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy = coerce $ VG.basicUnsafeCopy @vector @rep
  {-# INLINE basicUnsafeCopy #-}
  elemseq (VectorC vec) a = VG.elemseq vec (coerce a)
  {-# INLINE elemseq #-}
