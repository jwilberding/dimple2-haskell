{-# LANGUAGE DeriveDataTypeable #-}
module Dimple2.Base (
  Options(..)
  , Context(..)
  , Dimple2Error (..)
  , defaultOptions
  )where

import Data.Typeable (Typeable)
import Data.Data (Data)
import Dimple2.MulticastGroup

data Environment = Prod | Dev
                 deriving (Show,Eq,Data,Typeable)

data Options = Options {environment :: Environment,
                        multicastGroup :: MulticastGroup,
                        multicastPort :: Integer,
                        apiHost :: Maybe String,
                        apiPort :: Integer,
                        localViewSize :: Int,
                        shuffleInterval :: Int,
                        interestWait :: Int,
                        automaticDiscovery :: Bool}
             deriving (Show,Eq,Data,Typeable)

defaultOptions :: Options
defaultOptions = Options {environment = Prod,
                          multicastGroup = read "231.10.10.25",
                          multicastPort = 29135,
                          apiHost = Nothing,
                          apiPort = 47688,
                          localViewSize = 10,
                          shuffleInterval = 10,
                          interestWait = 10,
                          automaticDiscovery = True}

data Context = Context {peerURL :: String,
                        options :: Options}
                 deriving (Show)

data Dimple2Error = InvalidAPIHost
                  deriving (Show, Eq)
