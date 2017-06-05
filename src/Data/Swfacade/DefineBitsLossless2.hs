{-# LANGUAGE ScopedTypeVariables #-}
module Data.Swfacade.DefineBitsLossless2 where

import Vision.Image
import Vision.Primitive
import Data.Binary.Get
import Control.Monad
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Ix as Ix
import qualified Data.Vector as V
import qualified Vision.Image.Storage.DevIL as DevIL
import Data.Word
import Text.Printf
import Data.Swfacade.RawTag

data DefineBitsLossless2 = DefineBitsLossless2
  { characterId :: Int
  , imageData :: RGBA
  }

isDefineBitsLossless2 :: RawTag -> Bool
isDefineBitsLossless2 = (== 36) . code

process :: RawTag -> IO ()
process rt
  | not (isDefineBitsLossless2 rt) = pure ()
process rt = do
    let raw = rawData rt
        getResult ::
            Either
              (LBS.ByteString, ByteOffset, String)
              (LBS.ByteString, ByteOffset, DefineBitsLossless2)
        getResult = runGetOrFail getData raw
    putStrLn "++++ PNG"
    case getResult of
        Left (_,offset,err) -> putStrLn ("Error: " ++ show err ++ " offset: " ++ show offset)
        Right (_, _, d) -> do
            let fp :: String
                fp = printf "extract-test-%d.png" (characterId d)
                img = imageData d
            putStr ("chId=" ++ show (characterId d) ++ " ")
            DevIL.save DevIL.PNG fp img
            putStrLn "good"
    putStrLn "---- PNG"

withDecompressedData :: forall a. (LBS.ByteString -> Get a) -> Get a
withDecompressedData getter = do
    compressed <- getRemainingLazyByteString
    let decompressed = Zlib.decompress compressed
        -- provide the decompressed copy in case getter wants to get full access of it
        -- for perhaps length checking
        getResult = runGetOrFail (getter decompressed) decompressed
    case getResult of
      Left (_,_,err) -> fail ("Error while acting on decompressed data: " ++ show err)
      Right (_,_,result) -> pure result

getData :: Get DefineBitsLossless2
getData = do
    chId <- fromIntegral <$> getWord16le
    bmpFmt <- getWord8
    when (bmpFmt `notElem` [3,5]) $
      fail "Unexpected bitmap format"
    w <- fromIntegral <$> getWord16le
    h <- fromIntegral <$> getWord16le
    let sp = ix2 h w
        getAlphaColormapData :: Int -> Get RGBA
        getAlphaColormapData ctSize = do
            colorTable <- V.fromList <$> replicateM (ctSize+1) getRGBA
            let extraW =
                    let (_,r) = w `quotRem` 4
                    in if r == 0 then 0 else 4 - r
                paddedW = w + extraW
                getImgLine = do
                    (xs,zeros) <- splitAt w <$> replicateM paddedW getWord8
                    -- might generate a warning?
                    --- when (any (/= 0) zeros) $ fail "expecting paddings to be zeros"
                    pure (V.fromList xs)
            (imgLines :: V.Vector (V.Vector Word8)) <- V.fromList <$> replicateM h getImgLine
            let getColor :: Point -> RGBAPixel
                getColor (Z :. y :. x) = colorTable V.! fromIntegral ind
                  where
                    ind :: Word8
                    ind = (imgLines V.! y) V.! x
            pure (fromFunction sp getColor)
          where
            getRGBA :: Get RGBAPixel
            getRGBA = RGBAPixel <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8

        getAlphaBitmapData :: Get RGBA
        getAlphaBitmapData = do
            withDecompressedData $ \decompressed -> do
                let expectedLen = h * w * 4
                    actualLen = LBS.length decompressed
                    getPixelInd = Ix.index ((0,0),(h-1,w-1))
                    getColor :: Point -> RGBAPixel
                    getColor (Z :. y :. x) = RGBAPixel r g b a
                      where
                        pInd = getPixelInd (y,x)
                        [a,r,g,b] =
                            (\colorInd ->
                             let ind = fromIntegral (colorInd + pInd*4)
                             in LBS.index decompressed ind) <$> [0,1,2,3]
                when (expectedLen /= fromIntegral actualLen) $
                    fail "Unexpected alpha bitmap data len"
                pure (fromFunction sp getColor)
    img <- case bmpFmt of
        3 -> do
            ctSize <- fromIntegral <$> getWord8
            withDecompressedData (const $ getAlphaColormapData ctSize)
        5 -> getAlphaBitmapData
        _ -> error "unreachable"

    pure (DefineBitsLossless2 chId img)
