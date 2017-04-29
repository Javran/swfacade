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
import Data.List.Split
import Data.Foldable
import Debug.Trace

data CompressMethod = Zlib | Lzma
  deriving Show

data Header = Header
  { compressMethod :: Maybe CompressMethod
  , version :: Int
  , contentLength :: Int
  , frameSize :: Rect
  , frameRate :: Double
  , frameCount :: Int
  } deriving Show

data RawTag = RawTag
  { code :: Word16
  , contentLength :: Int
  , rawData :: LBS.ByteString
  }

data Rect = Rect
  { nbits :: Int
  , xRange :: (Int, Int)
  , yRange :: (Int, Int)
  } deriving (Show)

w2c :: Word8 -> Char
w2c = toEnum . fromEnum

getChar8 :: Get Char
getChar8 = w2c <$> getWord8

getHeader1 :: Get (Maybe CompressMethod, (Int, Int))
getHeader1 = do
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
    pure (cm, (fromIntegral v, fromIntegral l))

getAll :: Get ((Maybe CompressMethod, (Int, Int)), LBS.ByteString)
getAll = do
    hd@(cm,_) <- getHeader1
    -- TODO: increase laziness
    remained <- getRemainingLazyByteString
    let decompress = case cm of
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

getRect :: Get Rect
getRect = do
    nbitsRaw <- getWord8
    let nbits :: Int
        nbits = fromIntegral (shiftR nbitsRaw 3 .&. 0x1F :: Word8)
        upper3 = nbitsRaw .&. 3
        neededBits = nbits*4 - 3
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
            . chunksOf nbits
            $ uppers ++ lowers
        packBits :: [Bool] -> Int
        packBits = foldl' (\acc i -> acc*2 + if i then 1 else 0) 0
    pure (Rect nbits (xMin,xMax) (yMin,yMax))

getHeader2 :: Get (Rect, (Double, Int))
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

getHeader :: LBS.ByteString -> (Header, LBS.ByteString)
getHeader raw = (Header cm v l rt fr fc, remained)
  where
    ((cm,(v,l)),decoded) = runGet getAll raw
    Right (remained, _, (rt,(fr,fc))) = runGetOrFail getHeader2 decoded

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
    let (hd,result) = getHeader raw
    print hd
    -- let (hd,result) = runGet getAll raw
    -- TODO: bring back length verification
    -- print (hd, LBS.length result+8)
    let ts = runGet getRawTags result
    print (length ts)
    mapM_ (putStrLn . explainCode . code) ts
    pure ()
