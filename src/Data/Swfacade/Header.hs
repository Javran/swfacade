{-# LANGUAGE
    DuplicateRecordFields
  , ScopedTypeVariables
  , PartialTypeSignatures
  , TypeFamilies
  #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Data.Swfacade.Header
  ( Rect(..)
  , Header(..)
  , getHeader
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Data.Word
import Control.Monad
import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Compression.Lzma as Lzma
import Data.Bits
import Data.List.Split
import Data.Foldable
import Control.Monad.Except
import Control.Arrow
import Data.Coerce
import Data.Functor.Identity

data CompressMethod = Zlib | Lzma
  deriving Show

data Rect = Rect
  { nbits :: Int
  , xRange :: (Int, Int)
  , yRange :: (Int, Int)
  } deriving (Show)

data Header = Header
  { compressMethod :: Maybe CompressMethod
  , version :: Int
  , fileLength :: Int
  , frameSize :: Rect
  , frameRate :: Double
  , frameCount :: Int
  } deriving Show

data CommonHeader = CommonHeader
  { compressMethod :: Maybe CompressMethod
  , version :: Int
  , fileLength :: Int
  } deriving Show

type BinaryParseError = (LBS.ByteString, ByteOffset, String)

data ParsePhase
  = PPCommonHeader
  | PPHeader2 -- second part of the header, which is part of the compressed data.
    deriving Show

data ParseError
  = ErrHeader ParsePhase BinaryParseError
  | ErrOther String
    deriving Show

type SwfParse = Except ParseError

w2c :: Word8 -> Char
w2c = toEnum . fromEnum

getChar8 :: Get Char
getChar8 = w2c <$> getWord8

runGetOrFail' ::
       (BinaryParseError -> ParseError)
    -> Get a -> LBS.ByteString
    -> SwfParse (LBS.ByteString, ByteOffset, a)
runGetOrFail' wrapError get = coerce . left wrapError . runGetOrFail get

getRect :: Get (Rect,Int)
getRect = do
    -- for this first byte,
    -- first 5 bits of it indicate the bit length
    -- while the following 3 bits is part of the rectangle
    nbitsRaw <- getWord8 -- first byte
    let -- cut 3 lower bits
        nbs :: Int
        nbs = fromIntegral (shiftR nbitsRaw 3 .&. 0x1F :: Word8)
        -- the term is a bit confusing: "upper3" refers to the first 3 bits
        -- of the data payload, which is left over from "nbitsRaw"
        upper3 = nbitsRaw .&. 3
        neededBits = nbs*4 - 3
        neededBytes = ceiling (fromIntegral neededBits / 8 :: Double) :: Int
    lowersRaw <- replicateM neededBytes getWord8
    -- now that "upper3 ++ lowers" has all bits we need with some padding 0s in the end
    let uppers :: [Bool] -- 3 upper bits
        uppers = testBit upper3 <$> [2,1,0]
        unpackBits :: Word8 -> [Bool]
        unpackBits x = testBit x <$> [7,6..0]
        lowers = concatMap unpackBits lowersRaw
        -- TODO: checking all remaining bits are False
        [xMin,xMax,yMin,yMax] =
            map packBits
            . take 4
            . chunksOf nbs
            $ uppers ++ lowers
        packBits :: [Bool] -> Int
        packBits = foldl' (\acc i -> acc*2 + if i then 1 else 0) 0
    pure (Rect nbs (xMin,xMax) (yMin,yMax), neededBytes+1)

-- get common header section, which appears
-- in the very beginning of every swf file
getCommonHeader :: Get CommonHeader
getCommonHeader = do
    c <- getChar8
    -- first byte indicates the compress method
    cm <- case c of
        'F' -> pure Nothing
        'C' -> pure (Just Zlib)
        'Z' -> pure (Just Lzma)
        _ -> fail "unrecognized compression method"
    -- followed by two magic bits
    magic <- (,) <$> getChar8 <*> getChar8
    guard $ magic == ('W','S')
    -- version number
    v <- getWord8
    -- file length
    l <- getWord32le
    pure (CommonHeader cm (fromIntegral v) (fromIntegral l))

-- consume common header part, get rest of the data (lazily) decompressed
consumeCommonHeader :: LBS.ByteString -> SwfParse (CommonHeader, LBS.ByteString)
consumeCommonHeader raw = do
    (remained, _ :: ByteOffset, ch) <-
        runGetOrFail' (ErrHeader PPCommonHeader) getCommonHeader raw
    let cm = compressMethod (ch :: CommonHeader)
        decompress = case cm of
            Nothing -> id
            Just Zlib -> Zlib.decompress
            Just Lzma ->
                {- TODO: this is not working, need to adjust the data somehow -}
                Lzma.decompress
    pure (ch, decompress remained)

-- fully parse header of a SWF file, return a lazy ByteString of unconsumed part
getHeader :: LBS.ByteString -> SwfParse ((Header, Int), LBS.ByteString)
getHeader raw = do
    (CommonHeader
      { compressMethod = cm
      , version = v
      , fileLength = l},decoded) <- consumeCommonHeader raw -- takes 8 bytes
    (remained, _, ((rt,rtSize),(fr,fc))) <-
        runGetOrFail' (ErrHeader PPHeader2) getHeader2 decoded
    pure ((Header cm v l rt fr fc, 8 + rtSize + 4), remained)
  where
    getHeader2 :: Get ((Rect, Int), (Double, Int))
    getHeader2 = do
        rt <- getRect
        -- frame rate
        frAfter <- fromIntegral <$> getWord8 :: Get Int
        frBefore <- fromIntegral <$> getWord8 :: Get Int
        let fr :: Double
            fr = fromIntegral frBefore + fromIntegral frAfter / 0x10000
        -- frame count
        fc <- fromIntegral <$> getWord16le
        pure (rt,(fr,fc))
