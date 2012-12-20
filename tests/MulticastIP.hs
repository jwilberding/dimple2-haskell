{- |
Module      :  $Header$
Description :  Tests for the various ip address types in the system
Copyright   :  (c) Eric Merritt
License     :  Apache 2.0
-}
module MulticastIP (
  multicastIPTests
  ) where

import Test.QuickCheck
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Dimple2.IP
import Dimple2.MulticastGroup

genCommonValues :: Gen [Int]
genCommonValues = vectorOf 3 $ choose (0, 255)

instance Arbitrary IP where
  arbitrary = do
    octet1 <- choose (0,255)
    [octet2, octet3, octet4] <- genCommonValues
    case  makeIP (octet1, octet2, octet3, octet4) of
      Right ip -> return ip
      Left _ -> error "error building group"

instance Arbitrary MulticastGroup where
  arbitrary = do
    octet1 <- choose (224,239)
    [octet2, octet3, octet4] <- genCommonValues
    case  makeMulticastGroup (octet1, octet2, octet3, octet4) of
      Right val -> return val
      Left _ -> error "error resolving group"

data GoodOct = GoodOct Int
             deriving (Show,Eq)

instance Arbitrary GoodOct where
  arbitrary = do
    e <- choose (0,255)
    return (GoodOct e)

data GoodMGOct = GoodMGOct Int
             deriving (Show,Eq)

instance Arbitrary GoodMGOct where
  arbitrary = do
    e <- choose (224,239)
    return (GoodMGOct e)

data BadOct = BadOct Int
             deriving (Show,Eq)

instance Arbitrary BadOct where
  arbitrary = do
    lowBad <- choose (-536870912, -1)
    highBad <- choose (256, 536870911)
    oneof [return (BadOct lowBad),
           return (BadOct highBad)]

data BadMGOct = BadMGOct Int
              deriving (Show,Eq)

instance Arbitrary BadMGOct where
  arbitrary = do
    lowBad <- choose (0, 223)
    highBad <- choose (240, 255)
    oneof [return (BadMGOct lowBad),
           return (BadMGOct highBad)]


data BadStrOct = BadStrOct String
                 deriving (Show,Eq)

instance Arbitrary BadStrOct where
  arbitrary = do
    len <- choose (1,100)
    val <- vectorOf len $ elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")
    return $ BadStrOct val

testOctetFail :: ((Int, Int, Int, Int) -> Either IPError a) ->
                   Int -> Int -> Int -> Int -> Int -> Bool
testOctetFail makeFun octLoc o1 o2 o3 o4 =
  let failOct = case octLoc of
        1 -> o1
        2 -> o2
        3 -> o3
        4 -> o4
        _ -> error "invalid test spec"
  in case makeFun (o1, o2, o3, o4) of
    Left (InvalidOctet loc invalid)
      | invalid == failOct && loc == octLoc -> True
    _ -> False

testStringCnvFail :: (String -> Either IPError a) ->
                        Int -> Int -> Int -> Int -> Int -> Bool
testStringCnvFail makeFun octLoc o1 o2 o3 o4 =
  let failOct = case octLoc of
        1 -> o1
        2 -> o2
        3 -> o3
        4 -> o4
        _ -> error "invalid test spec"
  in case makeFun $ concat [show o1, ".", show o2, ".", show o3, ".", show o4] of
    Left (InvalidOctet loc invalid)
      | invalid == failOct && loc == octLoc -> True
    _ -> False

testStringOctetFail :: (String -> Either IPError a) ->
                          Int -> String -> String -> String -> String -> Bool
testStringOctetFail makeFun octLoc o1 o2 o3 o4 =
  let failOct = case octLoc of
        1 -> o1
        2 -> o2
        3 -> o3
        4 -> o4
        _ -> error "invalid test spec"
  in case makeFun $ concat [o1, ".", o2, ".", o3, ".", o4] of
    Left (InvalidOctetString loc invalid)
      | invalid == failOct && loc == octLoc -> True
    _ -> False

prop_invalid_string :: BadStrOct -> Bool
prop_invalid_string (BadStrOct badIp) =
  case ipFromString badIp of
    Left (InvalidString badStr)
      | badIp == badStr -> True
    _ -> False

prop_oct_fail :: (Int, (BadOct, GoodOct, GoodOct, GoodOct)) -> Bool
prop_oct_fail (loc, (BadOct bad, GoodOct o2, GoodOct o3, GoodOct o4)) =
  let actual = loc `mod` 4
  in case actual of
    0 -> testOctetFail makeIP 1 bad o2 o3 o4
    1 -> testOctetFail makeIP 2 o2 bad o3 o4
    2 -> testOctetFail makeIP 3 o2 o3 bad o4
    3 -> testOctetFail makeIP 4 o2 o3 o4 bad
    _ -> error "unexpected result"

prop_mg_oct_fail :: (BadMGOct, GoodOct, GoodOct, GoodOct) -> Bool
prop_mg_oct_fail (BadMGOct bad, GoodOct o2, GoodOct o3, GoodOct o4) =
  testOctetFail makeMulticastGroup 1 bad o2 o3 o4

prop_string_cnv_fail :: (Int, (BadOct, GoodOct, GoodOct, GoodOct)) -> Bool
prop_string_cnv_fail (loc, (BadOct bad, GoodOct o2, GoodOct o3, GoodOct o4)) =
  let actual = loc `mod` 4
  in case actual of
    0 -> testStringCnvFail ipFromString 1 bad o2 o3 o4
    1 -> testStringCnvFail ipFromString 2 o2 bad o3 o4
    2 -> testStringCnvFail ipFromString 3 o2 o3 bad o4
    3 -> testStringCnvFail ipFromString 4 o2 o3 o4 bad
    _ -> error "unexpected result"

prop_mg_string_cnv_fail :: (BadMGOct, GoodOct, GoodOct, GoodOct) -> Bool
prop_mg_string_cnv_fail  (BadMGOct bad, GoodOct o2, GoodOct o3, GoodOct o4) =
  testStringCnvFail multicastGroupFromString 1 bad o2 o3 o4

prop_mg_bad_cnv_fail :: (BadOct, GoodOct, GoodOct, GoodOct) -> Bool
prop_mg_bad_cnv_fail  (BadOct bad, GoodOct o2, GoodOct o3, GoodOct o4) =
  testStringCnvFail multicastGroupFromString 1 bad o2 o3 o4

prop_string_oct_fail :: (Int, (BadStrOct, GoodOct, GoodOct, GoodOct)) -> Bool
prop_string_oct_fail (loc, (BadStrOct bad, GoodOct o2, GoodOct o3, GoodOct o4)) =
  let actual = loc `mod` 4
      strO2 = show o2
      strO3 = show o3
      strO4 = show o4
  in case actual of
    0 -> testStringOctetFail ipFromString 1 bad strO2 strO3 strO4
    1 -> testStringOctetFail ipFromString 2 strO2 bad strO3 strO4
    2 -> testStringOctetFail ipFromString 3 strO2 strO3 bad strO4
    3 -> testStringOctetFail ipFromString 4 strO4 strO3 strO4 bad
    _ -> error "unexpected result"

prop_tuple_invertable :: (GoodOct, GoodOct, GoodOct, GoodOct) -> Bool
prop_tuple_invertable (GoodOct o1, GoodOct o2, GoodOct o3, GoodOct o4) =
  case makeIP (o1, o2, o3, o4) of
    Right ip ->
      let ipStr = ipToString ip
      in case ipFromString ipStr of
        Right ip2
          | ip == ip2 -> True
          | otherwise -> False
        _ -> False
    _ -> False

prop_mtuple_invertable :: (GoodMGOct, GoodOct, GoodOct, GoodOct) -> Bool
prop_mtuple_invertable (GoodMGOct o1, GoodOct o2, GoodOct o3, GoodOct o4) =
  case makeMulticastGroup (o1, o2, o3, o4) of
    Right group ->
      let groupStr = multicastGroupToString group
      in case multicastGroupFromString groupStr of
        Right group2
          | group == group2 -> True
          | otherwise       -> False
        _ -> False
    _ -> False

prop_ip_invertable :: IP -> Bool
prop_ip_invertable ip =
  let ipStr = ipToString ip
  in case ipFromString ipStr of
    Right ip2
      | ip == ip2 -> True
      | otherwise -> False
    _ -> False

prop_ip_read_show :: IP -> Bool
prop_ip_read_show ip =
  let ipStr = show ip
  in read ipStr == ip

prop_mg_read_show :: MulticastGroup -> Bool
prop_mg_read_show group =
  let groupStr = show group
  in read groupStr == group

prop_multicastgroup_invertable :: MulticastGroup -> Bool
prop_multicastgroup_invertable group =
  let groupStr = multicastGroupToString group
  in case multicastGroupFromString groupStr of
    Right group2
      | group == group2 -> True
      | otherwise       -> False
    _ -> False

prop_mg_from_ip :: (GoodMGOct, GoodOct, GoodOct, GoodOct) -> Bool
prop_mg_from_ip (GoodMGOct o1, GoodOct o2, GoodOct o3, GoodOct o4) =
  case makeIP (o1, o2, o3, o4) of
    Right ip ->
      let Right groupIP =  multicastGroupFromIP ip
          groupStr = multicastGroupToString groupIP
      in case multicastGroupFromString groupStr of
        Right group2
          | groupIP == group2 -> True
          | otherwise         -> False
        _ -> False
    _ -> False

prop_bad_mg_from_ip :: (BadMGOct, GoodOct, GoodOct, GoodOct) -> Bool
prop_bad_mg_from_ip (BadMGOct o1, GoodOct o2, GoodOct o3, GoodOct o4) =
  case makeIP (o1, o2, o3, o4) of
    Right ip ->
      case multicastGroupFromIP ip of
        Left (InvalidOctet 1 invalid)
          | invalid == o1 -> True
        _  -> False
    _ -> False


multicastIPTests :: [Test]
multicastIPTests = [testGroup "IP Tests"
         [testProperty "read show test" prop_ip_read_show,
          testProperty "ip invertable" prop_ip_invertable,
          testProperty "tuple ip invertable" prop_tuple_invertable,
          testProperty "octet fail" prop_oct_fail,
          testProperty "octet string fail" prop_string_oct_fail,
          testProperty "octet conversion fail" prop_string_cnv_fail,
          testProperty "Invalid String" prop_invalid_string],
         testGroup "MulticastGroup Tests"
         [testProperty "read show test" prop_mg_read_show,
          testProperty "invertable" prop_multicastgroup_invertable,
          testProperty "invertable from ip" prop_mg_from_ip,
          testProperty "bad string conv from ip" prop_mg_bad_cnv_fail,
          testProperty "invalid from ip" prop_bad_mg_from_ip,
          testProperty "tuple invertable" prop_mtuple_invertable,
          testProperty "octet fail" prop_mg_oct_fail,
          testProperty "octet conversion fail" prop_mg_string_cnv_fail]]
