module Data.Group where

import Data.Monoid

class Monoid a => Group a where
  inverse :: a -> a

instance Num a => Group (Sum a) where
  inverse = negate

instance Integral a => Group (Product a) where
  inverse (Product a) = Product $ 1 `div` a
