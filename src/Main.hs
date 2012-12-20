{- |
Module      :  $Header$
Description :  Tests for the various ip address types in the system
Copyright   :  (c) Eric Merritt
License     :  Apache 2.0
-}
module Main (
  main
  ) where

import System.Console.CmdArgs

import Paths_dimple2 (version)
import Data.Version (showVersion)

import Dimple2

dimple2Options :: Options
dimple2Options =
  Options {environment = (environment defaultOptions) &= opt (environment defaultOptions)
                         &= help "Provide a runtime env (mostly affects logging)",
           multicastGroup = (multicastGroup defaultOptions) &= typ "IP|HOST"
                            &= opt (multicastGroup defaultOptions)
                            &= help "Provide the multicast group to broadcast on",
           multicastPort = def &= opt (multicastPort defaultOptions)
                           &= help "Provide the multicast port to broadcast on",
           apiHost = def &= opt "127.0.0.1" &= typ "IP|HOST"
                     &= name "h"
                     &= help "The host to use for API calls",
           apiPort = def &= opt (apiPort defaultOptions)
                     &= name "p"
                     &= help "Provide the port for API calls",
           localViewSize = def &= opt (localViewSize defaultOptions)
                     &= help "The size of the current local view",
           shuffleInterval = def &= opt (shuffleInterval defaultOptions)
                     &= help "The number of seconds between shuffles",
           interestWait = def &= opt (interestWait defaultOptions)
                          &= help "The number of seconds to wait after a broadcast \
                              \before looking for interested peers",
           automaticDiscovery = (automaticDiscovery defaultOptions) &= ignore} &=
    verbosity &=
    help "Start up stand alone test program for dimple2" &=
    summary (concat ["dimple2 v", showVersion version, " (C) Eric Merritt"]) &=
    details ["Dimple2 provides distributed membership for epidemic protocols"]


main :: IO ()
main = do
  opts <- cmdArgs dimple2Options
  print opts
