{-# LANGUAGE ScopedTypeVariables #-}
module Data.Swfacade.DefineBitsJPEG3 where

import Data.Swfacade.RawTag
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Binary.Get

data DefineBitsJPEG3 = DefineBitsJPEG3
  { characterId :: Int
  , imageData :: BS.ByteString
  , bitmapAlphaData :: Maybe BS.ByteString
  }

isDefineBitsJPEG3 :: RawTag -> Bool
isDefineBitsJPEG3 = (== 35) . code

process :: RawTag -> IO ()
process rt
  | not (isDefineBitsJPEG3 rt) = pure ()
process rt = do
    let xs = pprRawData (rawData rt)
    putStrLn "++++"
    mapM_ putStrLn xs
    putStrLn "----"

jpgSig :: BS.ByteString
jpgSig = BS.pack [0xFF, 0xD8]

pngSig :: BS.ByteString
pngSig = BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

gif89aSig :: BS.ByteString
gif89aSig = BS.pack [0x47, 0x49, 0x46, 0x38, 0x39, 0x61]

getData :: Get DefineBitsJPEG3
getData = do
    chId <- fromIntegral <$> getWord16le
    alphaOffset <- fromIntegral <$> getWord32le
    imgData <- getByteString alphaOffset
    pure $ DefineBitsJPEG3 chId imgData ({- TODO -} Nothing)
