-- V2: couple of experiments are commented out, yet the most important one is kept. It is faster to
-- call C.getContents and parse the ByteString. I learned here how ByteStrings internally work, and
-- it makes sense to make a huge buffer of all inputs. I do not know the effects on memory usage.
-- There is another option to use LazyByteStrings, but they are a little slower. I don't know if
-- there is any way to use this technique in interactive situations.

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

isSpace = isSpaceChar8

fastEval :: StateT [Int] IO a -> IO a
fastEval st = do
  hSetBuffering stdin (BlockBuffering Nothing)
  s <- getState'
  evalStateT st s 
  where
    getState =
      map (fst . fromJust . C.readInt) . C.words <$> C.getContents
    getState' = parse <$> C.getContents where
      parse = unfoldr go
      go s = do
        (n, s') <- C.readInt s
        let s'' = C.dropWhile isSpaceChar8 s'
        return (n, s'')
    getState'' = C.words <$> C.getContents where
      parse = unfoldr go
      go s = Just (t, ts) where
        (t, ts') = C.span (not . isSpace) s
        ts = C.tail ts'
    getState''' = C.getContents

pop :: StateT [Int] IO Int
pop = StateT go where
  go [] = error "empty list!"
  go (!x:xs) = pure (x, xs)

-- pop :: StateT [C.ByteString] IO Int
-- pop = StateT go where
--   go [] = error "empty list!"
--   go (!x:xs) = pure (n, xs) where
--     Just (n, _) = C.readInt x

-- pop :: StateT C.ByteString IO Int
-- pop = StateT go where
--   go s = pure (n, s'') where
--     Just (n, s') = C.readInt s
--     s'' = C.dropWhile isSpaceChar8 s'

readInts :: IO [Int]
readInts = map (fst . fromJust . C.readInt) . C.words <$> C.getLine

readInts' :: IO [Int]
readInts' = parse <$> C.getLine where
  parse = unfoldr go
  go s = do
    (n, s') <- C.readInt s
    let s'' = C.dropWhile isSpace s'
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

-- main = do
--   hSetBuffering stdout (BlockBuffering Nothing)
--   [n] <- readInts'
--   fen <- newFen @(ArrayC IOUArray Int) @(Sum Int) n
--   let go = do
--         typ : query <- readInts'
--         case typ of
--           0 -> do
--             let [p, x] = query
--             addFen fen p (Sum x)
--           1 -> do
--             let [r] = query
--             sumPrefixFen fen r >>= printInt . getSum
--           2 -> do
--             let [l, r] = query
--             sumRangeFen fen l r >>= printInt . getSum
--           _ -> pure ()
--         when (typ /= -1) go
--   go

main = fastEval $ do
  lift $ hSetBuffering stdout (BlockBuffering Nothing)
  n <- pop
  fen <- lift $ newFen @(ArrayC IOUArray Int) @(Sum Int) n
  let go = do
        typ <- pop
        case typ of
          0 -> do
            p <- pop
            x <- pop
            lift $ addFen fen p (Sum x)
          1 -> do
            r <- pop
            lift $ sumPrefixFen fen r >>= printInt . getSum
          2 -> do
            l <- pop
            r <- pop
            lift $ sumRangeFen fen l r >>= printInt . getSum
          _ -> pure ()
        when (typ /= -1) go
  go
