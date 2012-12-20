{-# LANGUAGE DeriveDataTypeable #-}
{- |
Module      :  $Header$
Description :  Provide a simple implementation of an IP address
Copyright   :  (c) Eric Merritt
License     :  Apache 2.0

Provides an abstract representation of an IP address for use with the
service registry. This provides validation of octet ranges and to/from
string values.

-}
module Dimple2.IP (
  IP
  , IPError(..)
  , makeIP
  , ipFromString
  , ipToString
  , ipToTuple
  ) where


import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Control.Arrow (second)

data IPError = InvalidOctet Int Int
             | InvalidOctetString Int String
             | InvalidString String
             deriving (Show,Eq)

data IP = IP Int Int Int Int
        deriving (Eq,Data,Typeable)

instance Read IP where
  readsPrec _ input =
    readCount (4::Int) input []
    where readCount 1 input' acc =
            let (strOctet4, rest) = span isDigit input'
                [octet1, octet2, octet3] = (reverse acc)
            in case makeIP (octet1, octet2, octet3, read strOctet4) of
              Left _ -> error "Prelude.read no parse"
              Right val -> [(val, rest)]
          readCount count input' acc =
            case second (span (== '.')) $ span isDigit input' of
              (octet, (".", rest))
                | not (null octet) -> acc `seq` readCount (count - 1) rest (read octet : acc)
              _ ->  error "Prelude.read no parse"

instance Show IP where
  show =
    ipToString

makeIP :: (Int, Int, Int, Int) ->
          Either IPError IP
makeIP (octet1, octet2, octet3, octet4)
  | octet1 < 0 || octet1 > 255   = Left $ InvalidOctet 1 octet1
  | octet2 < 0 || octet2 > 255   = Left $ InvalidOctet 2 octet2
  | octet3 < 0 || octet3 > 255   = Left $ InvalidOctet 3 octet3
  | octet4 < 0 || octet4 > 255   = Left $ InvalidOctet 4 octet4
  | otherwise                    = Right $ IP octet1 octet2 octet3 octet4

ipFromString :: String -> Either IPError IP
ipFromString str =
  case splitOn "." str of
    octet1:octet2:octet3:octet4:_ -> tupilify [octet1, octet2, octet3, octet4] []
    _                             -> Left $ InvalidString str
  where tupilify [] [octet4, octet3, octet2, octet1] =
          makeIP (octet1, octet2, octet3, octet4)
        tupilify [] _ =
          Left $ InvalidString str
        tupilify (x:xs) acc =
          case reads x of
            [(octet, _)] -> tupilify xs (octet:acc)
            _            -> Left $ InvalidOctetString (1 + length acc) x

ipToString :: IP -> String
ipToString (IP octet1 octet2 octet3 octet4) =
  concat [show octet1, ".", show octet2, ".", show octet3, ".", show octet4]

ipToTuple :: IP -> (Int, Int, Int, Int)
ipToTuple (IP  octet1 octet2 octet3 octet4) =
  (octet1, octet2, octet3, octet4)
