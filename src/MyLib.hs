{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module MyLib 
  ( module MyLib
  , module Data.Fenwick.Array
  , module Control.Monad
  , module Data.Monoid
  ) where

import Data.Array.ArrayC
import Data.Array.IO
import Data.Fenwick.Array
import Debug.Trace
import Data.Monoid
import Control.Monad

setup1 :: IO (FenMArray (ArrayC IOUArray Int) (Sum Int))
setup1 = do
  fen <- newFen @(ArrayC IOUArray Int) @(Sum Int) 10
  addFen fen 1 1
  addFen fen 3 1
  addFen fen 4 1
  addFen fen 8 1
  addFen fen 10 1
  debug fen
  pure fen

toSet :: MArray array (Sum Int) m => Int -> [Int] -> m (FenMArray array (Sum Int))
toSet n xs = newAccumFen n $ zip xs $ repeat $ Sum (1 :: Int)

setup2 :: Int -> [Int] -> IO (FenMArray (ArrayC IOUArray Int) (Sum Int))
setup2 = toSet @(ArrayC IOUArray Int) @IO

debug :: (MArray array (Sum a) IO, Num a, Show a) => FenMArray array (Sum a) -> IO ()
debug fen = forM_ [0..10] $ \i -> do
  Sum s <- sumPrefixFen fen i
  putStrLn $ "i = " <> show i <> ", s = " <> show s
