{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Tide.Client.Render where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Data.Foldable
import Data.Monoid

import Graphics.GPipe

import Tide.Client.Render.Resource
import Tide.Client.Render.Shader
import Tide.Types

bkData :: [(V4 Float, V2 Float)]
bkData =
  [ (V4 (-1) 1 0 1, V2 0 1)
  , (V4 1 1 0 1, V2 1 1)
  , (V4 1 (-1) 0 1, V2 1 0)
  , (V4 (-1) (-1) 0 1, V2 0 0)
  ]

player1Data :: [(V2 Float, V2 Float)]
player1Data = toTriangleList $
  zip
    ((V2 (V2 2 0) (V2 0 8) !*) <$> [V2 (-1) 1, V2 1 1, V2 1 (-1), V2 (-1) (-1)])
    [V2 1 0, V2 0 0, V2 0 1, V2 1 1]

player2Data :: [(V2 Float, V2 Float)]
player2Data =
  zip
    ((V2 (V2 2 0) (V2 0 8) !*) <$> [V2 (-1) 1, V2 1 1, V2 1 (-1), V2 (-1) (-1)])
    [V2 1 1, V2 0 1, V2 0 0, V2 1 0]

bulletData :: [V2 Float]
bulletData =
  (\d -> (V2 (sin d) (cos d))) <$> (\i -> fromIntegral i * 2 * pi / 10) <$>
    (toTriangleList [0 .. 10 :: Int])

toTriangleList :: [a] -> [a]
toTriangleList (v1 : vs) = concat $ zipWith (\v2 v3 -> [v1, v2, v3]) vs (tail vs)
toTriangleList _ = []

renderDevice ::
     ContextHandler ctx
  => Window os RGBAFloat ds
  -> IO DisplayData
  -> ContextT ctx os IO b
renderDevice win displayData = do
  bkVB <- initVertexBuffer bkData
  bkTex <- initTexture2D bkPic
  bkShader <- compileShader $ backgroundShader bkTex win

  p1VB <- initVertexBuffer player1Data
  player1Buffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 1
  tex1 <- initTexture2D girlPic1
  p1Shader <- compileShader $ playerShader player1Buffer tex1 win

  p2VB <- initVertexBuffer player1Data
  player2Buffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 1
  tex2 <- initTexture2D girlPic2
  p2Shader <- compileShader $ playerShader player2Buffer tex2 win

  bVB <- newBuffer (100 * 2000)
  bShader <- compileShader $ bulletShader win

  forever $ do
    (DisplayData d1 d2 bs) :: DisplayData <- lift displayData
    render $ do clearWindowColor win (V4 1 1 1 1)

    render $ runShader bkVB bkShader

    writeBuffer player1Buffer 0 [(realToFrac :: Double -> Float) <$> d1]
    render $ runShader p1VB p1Shader

    writeBuffer player2Buffer 0 [(realToFrac :: Double -> Float) <$> d2]
    render $ runShader p2VB p2Shader

    writeBuffer bVB 0 $ ((\p -> zip bulletData $ repeat (fmap (realToFrac :: Double -> Float) p)) =<< bs) <> replicate 100 (0,0)
    render $ runShader bVB bShader

    swapWindowBuffers win

initVertexBuffer ::
     (ContextHandler ctx, MonadIO m, BufferFormat b)
  => [HostFormat b]
  -> ContextT ctx os m (Buffer os b)
initVertexBuffer d = do
  vb <- newBuffer (length d)
  writeBuffer vb 0 d
  return vb

initTexture2D ::
     ( ContextHandler ctx
     , MonadIO m
     , BufferFormat b
     , ColorSampleable c
     , BufferColor (Color c (ColorElement c)) h ~ b
     , h ~ HostFormat b
     )
  => (Format c, Size2, [h])
  -> ContextT ctx os m (Texture2D os (Format c))
initTexture2D (format, wh, d) = do
  tex <- newTexture2D format wh 1
  writeTexture2D tex 0 0 wh d
  return tex

runShader :: Buffer os b -> CompiledShader os (PrimitiveArray Triangles b) -> Render os ()
runShader vb shader = do
  va <- newVertexArray vb
  let pa = toPrimitiveArray TriangleList va
  shader pa
