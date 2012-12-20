{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  $Header$
Description :  Provides a registry of services that is core to HMurrow
Copyright   :  (c) Eric Merritt
License     :  Apache 2.0

This module provides a means to store service endpoint information
along with service tags and query that information as needed.

-}
module Dimple2.REST (
  ) where


import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Dimple2

startAPI options =
  scotty 3000 $ do
    case environment options of
      Dev -> middleware logStdoutDev
      Prod -> middleware logStdout
