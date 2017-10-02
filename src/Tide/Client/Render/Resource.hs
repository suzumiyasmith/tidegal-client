{-# LANGUAGE TemplateHaskell #-}

module Tide.Client.Render.Resource where

import Data.FileEmbed
import qualified Data.ByteString as B
import Codec.Picture

import Linear
import Data.List.Split
import qualified Data.Vector.Storable as S
import Data.Word

import Graphics.GPipe

procPicture :: B.ByteString -> (Format RGBAFloat, V2 Int, [V4 Float])
procPicture raw = (RGBA8, V2 w h, dd)  where
  Right dPic = decodePng raw
  Image w h ds = convertRGBA8 dPic
  dd =
    fmap (\[r, g, b, a] -> V4 r g b a) $
    chunksOf 4 $
    fmap (\s -> fromIntegral s / fromIntegral (maxBound :: Word8)) $
    S.toList ds

girlPic1 = procPicture $(embedFile "res/chris.png")
girlPic2 = procPicture $(embedFile "res/chris2.png")

bkPic :: (Format RFloat, V2 Int, [Word32])
bkPic = (R8, V2 8 8, cycle $ take 8 whiteBlack ++ take 8 blackWhite) where
  whiteBlack = cycle [minBound, maxBound] :: [Word32]
  blackWhite = tail whiteBlack
