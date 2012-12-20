{-# LANGUAGE DeriveDataTypeable #-}
{- |
Module      :  $Header$
Description :  provides support for peer discovery
Copyright   :  (c) Eric Merritt
License     :  Apache 2.0


-}
module Dimple2.MulticastGroup (
  MulticastGroup
  , makeMulticastGroup
  , multicastGroupFromString
  , multicastGroupToString
  , multicastGroupFromIP
  ) where

import Data.Typeable (Typeable)
import Data.Data (Data)

import Dimple2.IP

data MulticastGroup = MulticastGroup IP
                      deriving (Eq,Data,Typeable)

instance Read MulticastGroup where
  readsPrec p input =
    case readsPrec p input of
      [(ip, rest)] -> case multicastGroupFromIP ip of
        Left _ -> error "Prelude.read no parse2"
        Right val -> [(val, rest)]
      _ -> error "Prelude.read no parse1"

instance Show MulticastGroup where
  show = multicastGroupToString

validateMulticast :: Either IPError IP -> Either IPError MulticastGroup
validateMulticast (Right ip) =
  let (octet1, _, _, _) = ipToTuple ip
  in if  octet1 < 224 || octet1 > 239
     then Left $ InvalidOctet 1 octet1
     else Right $ MulticastGroup ip
validateMulticast (Left val) =
  Left val

makeMulticastGroup :: (Int, Int, Int, Int) ->
                      Either IPError MulticastGroup
makeMulticastGroup =
  validateMulticast . makeIP

multicastGroupFromIP :: IP -> Either IPError MulticastGroup
multicastGroupFromIP =
  validateMulticast . Right

multicastGroupFromString :: String -> Either IPError MulticastGroup
multicastGroupFromString =
  validateMulticast . ipFromString

multicastGroupToString :: MulticastGroup -> String
multicastGroupToString (MulticastGroup ip) =
  ipToString ip
