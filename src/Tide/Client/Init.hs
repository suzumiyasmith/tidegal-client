module Tide.Client.Init where

import Control.Monad

import Control.Concurrent
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Tide.Types
import Tide.Client.Input
import Tide.Client.Render

mainRender :: (Chan PlayerInput, MVar DisplayData) -> IO ()
mainRender (i, r) = void $ do
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGBA8) windowConfig
    inputDevice win i
    renderDevice win $ takeMVar r

windowConfig :: GLFW.WindowConfig
windowConfig = GLFW.WindowConfig 500 500 "Hola" Nothing [] Nothing
