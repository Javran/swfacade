module Data.Swfacade.RawTag where

import Data.Word
import Data.Binary.Get
import Data.Bits
import Data.List
import Data.List.Split
import qualified Data.ByteString.Lazy as LBS

data TagSize = TSShort | TSLong deriving (Show)

-- for uninterpreted data
data RawTag = RawTag
  { code :: Word16
  , contentLength :: Int
  , rawData :: LBS.ByteString
  , tagSize :: TagSize
  }

-- compute how many bytes in total does this tag occupy,
-- including tag header
tagLength :: RawTag -> Int
tagLength t = contentLength t + case tagSize t of
    TSShort -> 2
    TSLong -> 6

getRawTag :: Get RawTag
getRawTag = do
    ((c,l),ts) <- getCodeAndLength
    raw <- getLazyByteString (fromIntegral l)
    pure (RawTag c l raw ts)
  where
    splitShort :: Word16 -> (Word16, Int)
    splitShort w = (upper10, fromIntegral lower6)
      where
        upper10 = shiftR w 6 .&. 0x3FF
        lower6 = w .&. 0x3F

    getCodeAndLength = do
        w <- getWord16le
        let (c,l) = splitShort w
        if l == 0x3F
          then do
            realLen <- getWord32le
            pure ((c,fromIntegral realLen),TSLong)
          else pure ((c,l),TSShort)

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


word8ToHex :: Word8 -> String
word8ToHex w = [table !! u, table !! l]
  where
    u, l :: Int
    (u,l) = fromIntegral w `quotRem` 16

    table = ['0'..'9'] ++ ['A'..'F']

-- try to dump data in HEX
pprRawData :: LBS.ByteString -> [String]
pprRawData = fmap processLine . chunksOf 16 . LBS.unpack
  where
    processLine :: [Word8] -> String
    processLine xs = unwords (processPair <$> chunksOf 2 xs)
    processPair = concatMap word8ToHex
