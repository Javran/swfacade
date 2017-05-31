module Data.Swfacade.DefineBitsJPEG3 where

import Data.Swfacade.RawTag

process :: RawTag -> IO ()
process rt
  | code rt /= 35 = pure ()
process rt = do
    let xs = pprRawData (rawData rt)
    mapM_ putStrLn xs
    pure ()
