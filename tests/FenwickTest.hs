{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeApplications, TupleSections #-}

module Main where

import Control.Monad
import Control.Monad.ST (RealWorld)
import Data.Array.ArrayC
import Data.Array.IO
import Data.List
import Data.Monoid
import Data.Proxy
import Data.Vector.VectorC
import Test.Hspec
import qualified Data.Fenwick.Array as FA
import qualified Data.Fenwick.Vector as FV
import qualified Data.Vector.Generic.Mutable as VG
import qualified Data.Vector.Unboxed.Mutable as VUM

class IsFenwick a where
  new :: Int -> IO a
  newAccum :: Foldable t => Int -> t (Int, Sum Int) -> IO a
  fromList :: Int -> [Sum Int] -> IO a
  getSize :: a -> Int
  increment :: a -> Int -> Sum Int -> IO ()
  sumPrefix :: a -> Int -> IO (Sum Int)
  lowerBound :: a -> Sum Int -> IO Int

type FenArray = FA.FenMArray (ArrayC IOUArray Int) (Sum Int)
type FenVector = FV.FenMVector (MVectorC VUM.MVector Int) RealWorld (Sum Int)

instance IsFenwick FenArray where
  new = FA.newFen
  newAccum = FA.newAccumFen
  fromList = FA.newListFen
  getSize = FA.getSizeFen
  increment = FA.addFen
  sumPrefix = FA.sumPrefixFen
  lowerBound = FA.lowerBoundFen

instance IsFenwick FenVector where
  new = FV.newFen
  newAccum = FV.newAccumFen
  fromList = FV.newListFen
  getSize = FV.getSizeFen
  increment = FV.addFen
  sumPrefix = FV.sumPrefixFen
  lowerBound = FV.lowerBoundFen

testFenwick :: forall a. IsFenwick a => Proxy a -> SpecWith ()
testFenwick _ = do
  it "has a fixed size" $ do
    fen <- new @a 100
    getSize fen `shouldBe` 100
  
  it "can compute partial sums" $ do
    let list = map Sum [3, -1, 4, 0, 9, -10, 8]
        n = length list
    fen <- fromList @a n list
    traverse (sumPrefix fen) [0..n] 
      `shouldReturn` scanl (<>) mempty list
  
  it "can be used as a set" $ do
    let n = 100
        list = [34, 10, 100, 63, 12, 6, 54, 29, 83]
        m = length list
        sorted = sort list
    fen <- newAccum @a n $ map (, Sum 1) list
    forM_ (zip [1..] sorted) $ \(i, x) -> do
      lowerBound fen (Sum i) `shouldReturn` x
  
  it "can be updated" $ do
    let n = 5
        allSumPrefix fen = traverse (sumPrefix fen) [1..n]
    fen <- new @a n
    increment fen 2 (Sum 2)
    allSumPrefix fen `shouldReturn` [0, 2, 2, 2, 2]
    increment fen 5 (Sum 3)
    allSumPrefix fen `shouldReturn` [0, 2, 2, 2, 5]
    increment fen 1 (Sum (-1))
    allSumPrefix fen `shouldReturn` [-1, 1, 1, 1, 4]
  
main :: IO ()
main = hspec $ do
  describe "Data.Fenwick.Array" $
    testFenwick (Proxy @FenArray)
  describe "Data.Fenwick.Vector" $ 
    testFenwick (Proxy @FenVector)
