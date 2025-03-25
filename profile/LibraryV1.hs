-- V1: about 10% improvment with unsafeRead, but the main bottelneck is IO. This is a better
-- alternative as (I guess) it avoids second loop that C.words requires. See
-- <https://stackoverflow.com/questions/43570129>

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.ByteString.Builder
import System.IO
import Data.List
import Data.Maybe
import Data.Array.IO
import Data.Monoid
import Control.Monad
import Data.Coerce
import Data.Semigroup.Commutative
import Control.Monad.State
import Debug.Trace
import Data.ByteString.Internal
import Data.IORef
import Data.ByteString.Internal

import Data.Array.ArrayC
import Data.Fenwick.Array

readInts :: IO [Int]
readInts = map (fst . fromJust . C.readInt) . C.words <$> C.getLine

readInts' :: IO [Int]
readInts' = parse <$> C.getLine where
  parse = unfoldr go
  go s = do
    (n, s') <- C.readInt s
    let s'' = C.dropWhile isSpaceChar8 s'
    return (n, s'')

printInts :: [Int] -> IO ()
printInts xs = hPutBuilder stdout (go <> char7 '\n') where
  go = mconcat . intersperse (char7 ' ') . map intDec $ xs

printInt :: Int -> IO ()
printInt x = hPutBuilder stdout (intDec x <> char7 '\n')

sumRangeFen fen l r = do
  Sum rs <- sumPrefixFen fen (r - 1)
  Sum ls <- sumPrefixFen fen (l - 1)
  pure . Sum $ rs - ls

main = do
  hSetBuffering stdout (BlockBuffering Nothing)
  hSetBuffering stdin (BlockBuffering Nothing)
  [n] <- readInts
  fen <- newFen @(ArrayC IOUArray Int) @(Sum Int) n
  let go = do
        typ : query <- readInts
        case typ of
          0 -> do
            let [p, x] = query
            addFen fen p (Sum x)
          1 -> do
            let [r] = query
            sumPrefixFen fen r >>= printInt . getSum
          2 -> do
            let [l, r] = query
            sumRangeFen fen l r >>= printInt . getSum
          _ -> pure ()
        when (typ /= -1) go
  go
