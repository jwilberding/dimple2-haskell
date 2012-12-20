module Dimple2 (
  Options(..)
  , Context
  , defaultOptions
  , start
  )where

import Dimple2.Discovery
import Dimple2.Base

validateHost :: Context -> Either Dimple2Error Context
validateHost ctx@Context {options = Options {apiHost = Just host, apiPort = port}} =
  Right $ ctx{ peerURL = concat ["http://", host, ":", show port]}
validateHost _ =
  Left InvalidAPIHost

defaultContext :: Options -> Context
defaultContext opts =
  Context { peerURL = "",
            options = opts}

start :: Options -> IO (Either Dimple2Error Context)
start opts =
  case validateHost $ defaultContext opts of
    Right ctx -> startDiscovery ctx
    err@(Left _) -> return err
