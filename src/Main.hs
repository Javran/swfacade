{-# LANGUAGE DuplicateRecordFields, PartialTypeSignatures #-}
module Main where

import System.Environment
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Data.Word
import Data.Bits

import Data.Swfacade.Header

data RawTag = RawTag
  { code :: Word16
  , contentLength :: Int
  , rawData :: LBS.ByteString
  }

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
