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
  -- basicUnsafeSlice = coerce $ VGM.basicUnsafeSlice @VU.MVector
  basicUnsafeSlice i n (MVectorC vec) = MVectorC (VGM.basicUnsafeSlice i n vec)
  -- basicOverlaps = coerce $ VGM.basicOverlaps @VU.MVector
  basicOverlaps (MVectorC vec1) (MVectorC vec2) = VGM.basicOverlaps vec1 vec2
  -- basicUnsafeNew = coerce $ VGM.basicUnsafeNew @VU.MVector
  basicUnsafeNew n = MVectorC <$> VGM.basicUnsafeNew n
  -- basicInitialize = coerce $ VGM.basicInitialize @VU.MVector
  basicInitialize (MVectorC vec) = VGM.basicInitialize vec
  -- basicUnsafeReplicate = coerce $ VGM.basicUnsafeReplicate @VU.MVector
  basicUnsafeReplicate n a = MVectorC <$> VGM.basicUnsafeReplicate n (coerce a)
  -- basicUnsafeRead = coerce $ VGM.basicUnsafeRead @VU.MVector
  basicUnsafeRead (MVectorC vec) i = coerce <$> VGM.basicUnsafeRead vec i
  -- basicUnsafeWrite = coerce $ VGM.basicUnsafeWrite @VU.MVector
  basicUnsafeWrite (MVectorC vec) i a = VGM.basicUnsafeWrite vec i (coerce a)
  -- basicClear = coerce $ VGM.basicClear @VU.MVector
  basicClear (MVectorC vec) = VGM.basicClear vec
  -- basicSet = coerce $ VGM.basicSet @VU.MVector
  basicSet (MVectorC vec) a = VGM.basicSet vec (coerce a)
  -- basicUnsafeCopy = coerce $ VGM.basicUnsafeCopy @VU.MVector
  basicUnsafeCopy (MVectorC vec1) (MVectorC vec2) = VGM.basicUnsafeCopy vec1 vec2
  -- basicUnsafeMove = coerce $ VGM.basicUnsafeMove @VU.MVector
  basicUnsafeMove (MVectorC vec1) (MVectorC vec2) = VGM.basicUnsafeMove vec1 vec2
  -- basicUnsafeGrow = coerce $ VGM.basicUnsafeGrow @VU.MVector
  basicUnsafeGrow (MVectorC vec) n = MVectorC <$> VGM.basicUnsafeGrow vec n

instance (Coercible elem rep, VG.Vector vector rep) => VG.Vector (VectorC vector rep) elem where
  basicUnsafeFreeze = coerce $ VG.basicUnsafeFreeze @vector @rep
  basicUnsafeThaw = coerce $ VG.basicUnsafeThaw @vector @rep
  basicLength = coerce $ VG.basicLength @vector @rep
  basicUnsafeSlice = coerce $ VG.basicUnsafeSlice @vector @rep
  basicUnsafeIndexM = coerce $ VG.basicUnsafeIndexM @vector @rep
  basicUnsafeCopy = coerce $ VG.basicUnsafeCopy @vector @rep
  elemseq (VectorC vec) a = VG.elemseq vec (coerce a)
