module Utils where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Time (NominalDiffTime, secondsToNominalDiffTime)

toBSSlow :: (Show a) => a -> ByteString
toBSSlow = pack . show

milliSecontsToNominalDiffTime :: Integer -> NominalDiffTime
milliSecontsToNominalDiffTime = secondsToNominalDiffTime . (/ 1000) . fromIntegral
