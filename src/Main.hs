module Main where

import System.Environment
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Data.Coerce
import Data.Word
import Control.Monad

data CompressMethod = Zlib | Lzma

data Header = Header
  { compressMethod :: Maybe CompressMethod
  , version :: Int
  , contentLength :: Int
  }

w2c :: Word8 -> Char
w2c = toEnum . fromEnum

getChar8 :: Get Char
getChar8 = w2c <$> getWord8

getHeader :: Get Header
getHeader = do
    c <- getChar8
    cm <- case c of
        'F' -> pure Nothing
        'C' -> pure (Just Zlib)
        'Z' -> pure (Just Lzma)
        _ -> fail "unrecognized compression method"
    magic <- (,) <$> getChar8 <*> getChar8
    guard $ magic == ('W','S')
    v <- getWord8
    l <- getWord32le
    pure (Header cm (fromIntegral v) (fromIntegral l))

main :: IO ()
main = do
    [fp] <- getArgs
    raw <- LBS.readFile fp
    pure ()
