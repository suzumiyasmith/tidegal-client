module Tide.Client.Input where

import Control.Monad

import Control.Concurrent
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Tide.Types

inputDevice :: Window os c ds -> Chan PlayerInput -> ContextT GLFW.Handle os IO ()
inputDevice w inputChan = void $
  GLFW.setKeyCallback w (Just $ getInput inputChan)

getInput :: Chan PlayerInput -> GLFW.Key -> t1 -> GLFW.KeyState -> t -> IO ()
getInput i k _ st _ =
  case (st, commandMap k) of
    (GLFW.KeyState'Pressed, Just cmd)  -> writeChan i $ (cmd, True)
    (GLFW.KeyState'Released, Just cmd) -> writeChan i $ (cmd, False)
    _ -> return ()

commandMap :: GLFW.Key -> Maybe CommandType
commandMap k =
  case k of
    GLFW.Key'W ->
      Just MoveUp
    GLFW.Key'A ->
      Just MoveLeft
    GLFW.Key'S ->
      Just MoveDown
    GLFW.Key'D ->
      Just MoveRight
    _ -> Nothing
