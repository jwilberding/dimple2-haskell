{- |
Module      :  $Header$
Description :  Provides an implementation of automatic discovery
Copyright   :  (c) Eric Merritt
License     :  Apache 2.0

When the Dimple2 system is started it sends out a multicast broadcast
containing the URL in
[Netstrings](http://cr.yp.to/proto/netstrings.txt) format. Peers that
hear that broadcast should optionally `POST` their URL to the
`interested` peer API. Regardless of the number of nodes that hear
this broadcast no more then about 10 - 100 nodes should respond. This
implementation uses a probabilist algorythim based on the estimated
number of nodes in the system to ensure that property.

Algorythim

0. Check to see if there are any peers in the `localview`
1. If peers exist wait N seconds and try again
2. If no peers exist, Broadcast Peer URL in Netstrings format
4. Collect responses from interested parties for N seconds
5. If interested parties have responded, start join operation
6. If interested parties have not responded start broadcast process again

-}
module Dimple2.Discovery (
  startDiscovery
  ) where

import Dimple2.Base
import Control.Concurrent
import Network.Socket.ByteString
import Network.Multicast
import qualified Data.ByteString as ByteString
encode apiUrl =
  let value = String->ByteString
i

broadcast :: Context -> IO a
broadcast Context {options = Options {multicastGroup = group, multicastPort = port},
                   peerURL = apiURL} =
  withSocketsDo $ do
    (sock, addr) <- multicastSender (show group) (fromInteger port)
    let loop = do
          _ <- sendTo sock "Hello, world" addr
          loop in loop


startDiscovery :: Context -> IO (Either Dimple2Error Context)
startDiscovery ctx = do
  _ <- forkIO $ broadcast ctx;
  return $ Right ctx
