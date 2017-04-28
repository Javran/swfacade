{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import System.Environment
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Data.Word
import Control.Monad
import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Compression.Lzma as Lzma
import Data.Bits

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

getRawTag :: Get RawTag
getRawTag = do
    (c,l) <- getCodeAndLength
    raw <- getLazyByteString (fromIntegral l)
    pure (RawTag c l raw)
  where
    splitShort :: Word16 -> (Word16, Int)
    splitShort w = (upper10, fromIntegral lower6)
      where
        upper10 = shiftR w 6 .&. 0x3FF
        lower6 = w .&. 0x3F

    getCodeAndLength :: Get (Word16, Int)
    getCodeAndLength = do
        w <- getWord16le
        let (c,l) = splitShort w
        if l == 0x3F
          then do
            realLen <- getWord32le
            pure (c,fromIntegral realLen)
          else pure (c,l)

isEndTag :: RawTag -> Bool
isEndTag = (== 0) . code

getRawTags :: Get [RawTag]
getRawTags = do
    t <- getRawTag
    if isEndTag t
      then pure [t]
      else do
        ts <- getRawTags
        pure (t:ts)

passRect :: Get ()
passRect = do
    nbitsRaw <- getWord8
    let nbits = shiftR nbitsRaw 3 .&. 0x1F
        neededBits = nbits*4 - 3
        neededBytes = ceiling (fromIntegral neededBits / 8 :: Double) :: Int
    _ <- getLazyByteString (fromIntegral neededBytes)
    pure ()

passBoring :: Get ()
passBoring = do
    passRect
    -- frame rate
    _ <- getWord16le
    -- frame count
    _ <- getWord16le
    pure ()

explainCode :: Word16 -> String
explainCode c = show c ++ "\t" ++ explain
  where
    explain = case c of
        0 -> "End"
        1 -> "ShowFrame"
        2 -> "DefineShape"
        9 -> "SetBackgroundColor"
        22 -> "DefineShape2"
        35 -> "DefineBitsJPEG3"
        36 -> "DefineBitsLossless2"
        39 -> "DefineSprite"
        69 -> "FileAttributes"
        76 -> "SymbolClass"
        82 -> "DoABC"
        86 -> "DefineSceneAndFrameLabelData"
        _ -> "<Unknown>"

main :: IO ()
main = do
    [fp] <- getArgs
    raw <- LBS.readFile fp
    let (hd,result) = runGet getAll raw
    print (hd, LBS.length result+8)
    let ts = runGet (passBoring >> getRawTags) result
    print (length ts)
    mapM_ (putStrLn . explainCode . code) ts
    pure ()
