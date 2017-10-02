{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Tide.Client.Render.Shader where

import Data.Monoid
import Data.Word
import Graphics.GPipe

backgroundShader ::
     Texture2D os (Format RFloat)
  -> Window os RGBAFloat ds
  -> Shader os (PrimitiveArray Triangles (B4 Float, B2 Float)) ()
backgroundShader bkTex win = do
  primitiveStream :: PrimitiveStream Triangles (V4 VFloat, V2 VFloat) <-
    toPrimitiveStream id
  fragmentStream <-
    rasterize
      (const (FrontAndBack, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1))
      primitiveStream
  let filter = SamplerFilter Nearest Nearest Nearest Nothing
      edge = (pure Repeat, undefined)
  samp <- newSampler2D (const (bkTex, filter, edge))
  let sampleTexture =
        (\s -> V4 s s s 1) . sample2D samp SampleAuto Nothing Nothing
      fragmentStream2 = fmap sampleTexture fragmentStream
  drawWindowColor
    (const (win, ContextColorOption NoBlending (pure True)))
    fragmentStream2

playerShader ::
     Buffer os (Uniform (B3 Float))
  -> Texture2D os (Format RGBAFloat)
  -> Window os RGBAFloat ds
  -> Shader os (PrimitiveArray Triangles (B2 Float, B2 Float)) ()
playerShader playerBuffer tex win = do
  primitiveStream <- toPrimitiveStream id
  (V3 px py pr) <- getUniform (const (playerBuffer, 0))
  let primitiveStreamAll :: PrimitiveStream Triangles (V4 VFloat, V2 VFloat)
      primitiveStreamAll =
        (\(pos, uv) -> (transMatrix px py !* toV4 pos, rotateMatrix2D pr !* uv)) <$>
        primitiveStream
  fragmentStream <-
    rasterize
      (const (FrontAndBack, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1))
      primitiveStreamAll
  let filter = SamplerFilter Nearest Nearest Nearest Nothing
      edge = (pure Repeat, undefined)
  samp <- newSampler2D (const (tex, filter, edge))
  let
    sampleTexture :: V2 FFloat -> V4 FFloat
    sampleTexture = sample2D samp SampleAuto Nothing Nothing
    fragmentStream2 = fmap sampleTexture fragmentStream
  drawWindowColor
    (const (win, ContextColorOption simpleBlender (pure True)))
    fragmentStream2

bulletShader ::
     Window os RGBAFloat ds
  -> Shader os (PrimitiveArray Triangles (B2 Float, B3 Float)) ()
bulletShader win = do
  primitiveStream :: PrimitiveStream Triangles (V2 VFloat, V3 VFloat) <-
    toPrimitiveStream id
  let primitiveStream1 :: PrimitiveStream Triangles (V4 VFloat, V4 VFloat)
      primitiveStream1 =
        (\(pos, V3 bx by br) -> (transMatrix bx by !* toV4 pos, V4 1 0 1 1)) <$>
        primitiveStream
  fragmentStream <-
    rasterize
      (const (FrontAndBack, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1))
      primitiveStream1
  drawWindowColor
    (const (win, ContextColorOption simpleBlender (pure True)))
    fragmentStream

toV4 :: V2 VFloat -> V4 VFloat
toV4 (V2 x y) = V4 x y 0 1

transMatrix' x y r = scaleMatrix 0.02 !*! m2 !*! m1
  where
    m1 = V4 (V4 1 0 0 (x)) (V4 0 1 0 (y)) (V4 0 0 1 0) (V4 0 0 0 1)
    m2 = V4 (V4 (cos r) (- sin r) 0 0) (V4 (sin r) (cos r) 0 0) (V4 0 0 1 0) (V4 0 0 0 1)

transMatrix x y = scaleMatrix 0.02 !*! m1
  where
    m1 = V4 (V4 1 0 0 (x)) (V4 0 1 0 (y)) (V4 0 0 1 0) (V4 0 0 0 1)

scaleMatrix d = V4 (V4 d 0 0 0) (V4 0 d 0 0) (V4 0 0 1 0) (V4 0 0 0 1)

rotateMatrix2D r = V2 (V2 (cos r) (-sin r)) (V2 (sin r) (cos r))

simpleBlender :: Blending
simpleBlender =
  BlendRgbAlpha
    (FuncAdd, FuncAdd)
    (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
    (V4 0 0 0 0)
