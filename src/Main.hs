{-# LANGUAGE DuplicateRecordFields, PartialTypeSignatures, NamedFieldPuns #-}
module Main where

import System.Environment
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Data.Word
import Control.Monad.Except
import Data.Swfacade.Header
import Data.Swfacade.RawTag

-- import qualified Data.Swfacade.DefineBitsLossless2 as PNG
import qualified Data.Swfacade.DefineSound as Sound

verifyLength :: Header -> Int -> [RawTag] -> Either (Int,Int) Int
verifyLength header hdSize tags = if fullFileLength == hdSize + fullTagLength
    then Right fullFileLength
    else Left (fullFileLength, hdSize + fullTagLength)
  where
    fullFileLength = fileLength header
    fullTagLength = sum (map tagLength tags)

explainCode :: Word16 -> String
explainCode c = show c ++ "\t" ++ explain
  where
    explain = case c of
        0 -> "End"
        1 -> "ShowFrame"
        2 -> "DefineShape"
        9 -> "SetBackgroundColor"
        14 -> "DefineSound"
        20 -> "DefineBitsLossless"
        22 -> "DefineShape2"
        26 -> "PlaceObject2"
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
    let Right ((header,hdSize),result) = runExcept (getHeader raw)
    let ts :: [RawTag]
        ts = runGet getRawTags result
    forM_ ts $ \ RawTag {code, contentLength} ->
        putStrLn $ "code: " ++ show code ++ ", len: " ++ show contentLength
    print (length ts)
    print header
    mapM_ (putStrLn . explainCode . code) ts
    case verifyLength header hdSize ts of
        Left p -> putStrLn $ "File size mismatch: " ++ show p
        Right l -> putStrLn $ "Verified full file length: " ++ show l

    mapM_ Sound.process ts
