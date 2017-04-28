{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import System.Environment
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Data.Word
import Control.Monad
import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Compression.Lzma as Lzma

data CompressMethod = Zlib | Lzma
  deriving Show

data Header = Header
  { compressMethod :: Maybe CompressMethod
  , version :: Int
  , contentLength :: Int
  } deriving Show

data RawTag = RawTag
  { code :: Word16
  , contentLength :: Int
  , rawData :: LBS.ByteString
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

getAll :: Get (Header, LBS.ByteString)
getAll = do
    hd <- getHeader
    -- TODO: increase laziness
    remained <- getRemainingLazyByteString
    let decompress = case compressMethod hd of
            Nothing -> id
            Just Zlib -> Zlib.decompress
            Just Lzma -> Lzma.decompress
    let decoded = decompress remained
    pure (hd,decoded)

main :: IO ()
main = do
    [fp] <- getArgs
    raw <- LBS.readFile fp
    let (hd,result) = runGet getAll raw
    print (hd, LBS.length result+8)
    pure ()
