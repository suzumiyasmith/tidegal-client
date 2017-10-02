{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Tide.Client.Network where


import Control.Monad

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL

import Control.Concurrent

import Network.WebSockets
import Wuss

import Tide.Types
import Tide.Client.Init

mainClient = setClient >>= mainRender

setClient :: IO (Chan PlayerInput, MVar DisplayData)
setClient = do
  i :: Chan PlayerInput <- newChan
  r :: MVar DisplayData <- newEmptyMVar
  -- forkIO $ runSecureClientWith "fff.prpr.link" 443 "/" defaultConnectionOptions (userInfoHeader user1) $ comunicate i r
  forkIO $ runClientWith "127.0.0.1" 8888 "/" defaultConnectionOptions (userInfoHeader user1) $ comunicate i r
  return (i, r)


userInfoHeader :: UserInfo -> Headers
userInfoHeader u = [("User-Info", BL.toStrict $ B.encode $ u)]

user1 = UserInfo "haha1" "233"

comunicate :: Chan PlayerInput -> MVar DisplayData -> Connection -> IO ()
comunicate i r conn = do
  forkIO $ forever $ do
    rr :: DisplayData <- B.decode <$> receiveData conn
    void $ tryPutMVar r rr

  forkIO $ forever $ do
    ii <- readChan i
    sendBinaryData conn $ B.encode ii

  forkPingThread conn 30
  forever $ threadDelay 30000000
