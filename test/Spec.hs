{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
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
tests = [ jsonParserTests
        , genericJSONParserTests ]

jsonParserTests :: TF.Test
jsonParserTests = testGroup "\nJSON parser tests" . hUnitTestToTests $
  HU.TestList [ canParseValueFromResponse
              , canParsePreviousNodeValue
              , canExtractPairsOfJSONValues
              , canParseEventSetOfNode
              , canDistinguishSetFromDelete
              , canParseEventDeleteOfNode ]

genericJSONParserTests :: TF.Test
genericJSONParserTests = testGroup "Generic JSON Parser tests" . hUnitTestToTests $
  HU.TestList [ canExtractTextInJSBranch
              , canExtractIntegerInJSBranch
              , canExtractListInJSBranch
              , canExtractListInJSBranchWithTypeFamily ]



--------------------------------------------------------------------------------
-- JSON Parser tests
--------------------------------------------------------------------------------

canParseValueFromResponse :: HU.Test
canParseValueFromResponse = "Can obtain the value of a node from response" ~:
  let json = "{\"action\": \"get\", \"node\": { \"createdIndex\": 2," `B.append`
             "\"key\": \"/message\", \"modifiedIndex\": 2," `B.append`
             "\"value\": \"Hello world\" }}"
      expected = Just $ NodeValue "Hello world" :: Maybe (NodeValue Text)
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
      expected = Just $ Pair (NodeValue "Hello etcd") (PreviousValue "Hello world")
        :: Maybe (Pair (NodeValue Text) (PreviousValue Text))
  in
    expected @=? A.decode json
  

canParseEventSetOfNode :: HU.Test
canParseEventSetOfNode = "Can detect set action and parse information" ~:
  let json = "{\"action\":\"set\",\"node\":{" `B.append`
             "\"key\":\"/test\",\"value\":\"10\"," `B.append`
             "\"modifiedIndex\":6,\"createdIndex\":6}," `B.append`
             "\"prevNode\":{\"key\":\"/test\"," `B.append`
             "\"value\":\"11\",\"modifiedIndex\":5,\"createdIndex\":5}}"
      expected = Just . Action $ Pair (NodeValue "10") (PreviousValue "11")
        :: Maybe (Action "set" (Pair (NodeValue Text) (PreviousValue Text)))
  in
    expected @=? A.decode json


canDistinguishSetFromDelete :: HU.Test
canDistinguishSetFromDelete = "Can distinguish delete event from set" ~:
  let json = "{\"action\":\"set\",\"node\":{" `B.append`
             "\"key\":\"/test\",\"value\":\"10\"," `B.append`
             "\"modifiedIndex\":6,\"createdIndex\":6}," `B.append`
             "\"prevNode\":{\"key\":\"/test\"," `B.append`
             "\"value\":\"11\",\"modifiedIndex\":5,\"createdIndex\":5}}"
      expected = Nothing
        :: Maybe (Action "delete" (PreviousValue Text))
  in
    expected @=? A.decode json

canParseEventDeleteOfNode :: HU.Test
canParseEventDeleteOfNode = "Can detect delete action and parse information" ~:
  let json = "{\"action\":\"delete\",\"node\":" `B.append`
             "{\"key\":\"/test\",\"modifiedIndex\":9," `B.append`
             "\"createdIndex\":8},\"prevNode\":" `B.append`
             "{\"key\":\"/test\",\"value\":\"10\"," `B.append`
             "\"modifiedIndex\":8,\"createdIndex\":8}}"
      expected = Just . Action $ PreviousValue "10"
        :: Maybe (Action "delete" (PreviousValue Text))
  in
    expected @=? A.decode json



--------------------------------------------------------------------------------
-- Generic JSON Parser tests
--------------------------------------------------------------------------------

canExtractTextInJSBranch :: HU.Test
canExtractTextInJSBranch = "Can extract JSBranch on Text" ~:
  let json = "{\"key1\":{\"key2\":{\"key3\":\"Some text\"}}}"
      expected = Just "Some text"
      decoded = A.decode json :: Maybe (JSBranch '["key1", "key2", "key3"] Text)
  in
    expected @=? fmap unwrap decoded

canExtractIntegerInJSBranch :: HU.Test
canExtractIntegerInJSBranch = "Can extract JSBranch on Integer" ~:
  let json = "{\"key1\":{\"key2\":{\"key3\":42}}}"
      expected = Just 42
      decoded = A.decode json :: Maybe (JSBranch '["key1", "key2", "key3"] Integer)
  in
    expected @=? fmap unwrap decoded

canExtractListInJSBranch :: HU.Test
canExtractListInJSBranch = "Can extract JSBranch on List" ~:
  let json = "{\"key1\":[{\"key2\":41},{\"key2\":42}]}"
      expected = Just [41,42]
      decoded = A.decode json :: Maybe (JSBranch '["key1"] [JSBranch '["key2"] Integer])
  in
    expected @=? (fmap (fmap unwrap) . (fmap unwrap) $ decoded)


canExtractListInJSBranchWithTypeFamily :: HU.Test
canExtractListInJSBranchWithTypeFamily = "Can extract JSBranch on List with type family" ~:
  let json = "{\"key1\":[{\"key2\":41},{\"key2\":42}]}"
      expected = Just [41,42]
      decoded = A.decode json :: Maybe ("key1" |>| List ("key2" |>| Parse Integer))
  in
    expected @=? (fmap (fmap unwrap) . (fmap unwrap) $ decoded)
