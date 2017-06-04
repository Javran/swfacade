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
import Data.Word

data DefineBitsLossless2 = DefineBitsLossless2
  { characterId :: Int
  , imageData :: RGBA
  }

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
            let extraW = if w `mod` 4 == 0 then 0 else 4 - w `mod` 4
                paddedW = w + extraW
                getImgLine = do
                    (xs,zeros) <- splitAt w <$> replicateM paddedW getWord8
                    when (any (/= 0) zeros) $ fail "expecting paddings to be zeros"
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
            compressed <- getRemainingLazyByteString
            let decompressed = Zlib.decompress compressed
                expectedLen = h * w * 4
                actualLen = LBS.length decompressed
                getPixelInd = Ix.index ((0,0),(h-1,w-1))
                getColor :: Point -> RGBAPixel
                getColor (Z :. y :. x) = RGBAPixel r g b a
                  where
                    pInd = getPixelInd (y,x)
                    [a,r,g,b] =
                        (\colorInd ->
                         let ind = fromIntegral (colorInd + pInd*4)
                         in LBS.index decompressed ind) <$> [0..]

            when (expectedLen /= fromIntegral actualLen) $
                fail "Unexpected alpha bitmap data len"
            pure (fromFunction sp getColor)
    img <- case bmpFmt of
        3 -> do
            ctSize <- fromIntegral <$> getWord8
            getAlphaColormapData ctSize
        5 -> getAlphaBitmapData
        _ -> error "unreachable"

    pure (DefineBitsLossless2 chId img)
