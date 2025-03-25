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

import Data.Array.ArrayC
import Data.Fenwick.Array

printInts :: [Int] -> IO ()
printInts xs = hPutBuilder stdout (go <> char7 '\n') where
  go = mconcat . intersperse (char7 ' ') . map intDec $ xs

printInt :: Int -> IO ()
printInt x = hPutBuilder stdout (intDec x <> char7 '\n')

sumRangeFen fen l r = do
  Sum rs <- sumPrefixFen fen (r - 1)
  Sum ls <- sumPrefixFen fen (l - 1)
  pure . Sum $ rs - ls

withFastIO f = do
  hSetBuffering stdout (BlockBuffering Nothing)
  hSetBuffering stdin (BlockBuffering Nothing)
  input <- C.getContents
  inputRef <- newIORef input
  let pop g = do
        s <- readIORef inputRef
        let (!x, !s') = g s
            !s'' = C.dropWhile isSpaceChar8 (C.tail s')
        writeIORef inputRef s''
        pure x
  f pop

main = withFastIO $ \pop -> do
  let popInt = pop $ fromJust . C.readInt
  n <- popInt
  fen <- newFen @(ArrayC IOUArray Int) @(Sum Int) n
  let go = do
        typ <- popInt
        case typ of
          0 -> do
            p <- popInt
            x <- popInt
            addFen fen p (Sum x)
          1 -> do
            r <- popInt
            sumPrefixFen fen r >>= printInt . getSum
          2 -> do
            l <- popInt
            r <- popInt
            sumRangeFen fen l r >>= printInt . getSum
          _ -> pure ()
        when (typ /= -1) go
  go