{-# LANGUAGE OverloadedStrings #-}
import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Protolude
import Database.Etcd.JSON
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson as A
import Data.Text (Text)

main :: IO ()
main = defaultMain tests

tests :: [ TF.Test ]
tests = [ jsonParserTests ]

jsonParserTests :: TF.Test
jsonParserTests = testGroup "JSON parser tests" . hUnitTestToTests $
  HU.TestList [ canParseValueFromResponse
              , canParsePreviousNodeValue
              , canExtractPairsOfJSONValues ]

canParseValueFromResponse :: HU.Test
canParseValueFromResponse = "Can obtain the value of a node from response" ~:
  let json = "{\"action\": \"get\", \"node\": { \"createdIndex\": 2," `B.append`
             "\"key\": \"/message\", \"modifiedIndex\": 2," `B.append`
             "\"value\": \"Hello world\" }}"
      expected = Just $ Value "Hello world" :: Maybe (Value Text)
  in
    expected @=? A.decode json

canParsePreviousNodeValue :: HU.Test
canParsePreviousNodeValue = "Can parse previous node value from a response" ~:
  let json = "{\"action\": \"set\", \"node\": { \"createdIndex\": 3," `B.append`
             "\"key\": \"/message\", \"modifiedIndex\": 3," `B.append`
             "\"value\": \"Hello etcd\"}, \"prevNode\": {" `B.append`
             "\"createdIndex\": 2, \"key\": \"/message\"," `B.append`
             "\"value\": \"Hello world\", \"modifiedIndex\": 2}}"
      expected = Just $ PreviousValue "Hello world"
        :: Maybe (PreviousValue Text)
  in
    expected @=? A.decode json

canExtractPairsOfJSONValues :: HU.Test
canExtractPairsOfJSONValues = "Can extract pairs of JSON objects" ~:
  let json = "{\"action\": \"set\", \"node\": { \"createdIndex\": 3," `B.append`
             "\"key\": \"/message\", \"modifiedIndex\": 3," `B.append`
             "\"value\": \"Hello etcd\"}, \"prevNode\": {" `B.append`
             "\"createdIndex\": 2, \"key\": \"/message\"," `B.append`
             "\"value\": \"Hello world\", \"modifiedIndex\": 2}}"
      expected = Just $ Pair (Value "Hello etcd") (PreviousValue "Hello world")
        :: Maybe (Pair (Value Text) (PreviousValue Text))
  in
    expected @=? A.decode json
  
