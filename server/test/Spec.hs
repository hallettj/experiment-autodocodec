{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Autodocodec (eitherDecodeJSONViaCodec, toJSONViaCodec)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Either (isLeft)
import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "openApi" $ do
    it "serializes v1 example" $ do
      let output = V1 $ ExampleV1 {exampleTextField = Nothing, exampleIntField = 1}
      let expected = [aesonQQ|{ "version": 1, "text": null, "int": 1 }|]
      toJSONViaCodec output `shouldBe` expected

    it "serializes v2 example" $ do
      let output = V2 $ ExampleV2 {exampleV2TextField = "hello", exampleV2IntField = 1, exampleV2BoolField = True}
      let expected = [aesonQQ|{ "version": 2, "text": "hello", "int": 1, "bool": true }|]
      toJSONViaCodec output `shouldBe` expected

    it "parses v1 example" $ do
      let input = "{\"version\": 1, \"text\": null, \"int\": 1 }"
      let expected = V1 $ ExampleV1 {exampleTextField = Nothing, exampleIntField = 1}
      let actual = eitherDecodeJSONViaCodec input :: Either String Lib.Example
      actual `shouldBe` Right expected

    it "parses v2 example" $ do
      let input = "{\"version\": 2, \"text\": \"hello\", \"int\": 1, \"bool\": true }"
      let expected = V2 $ ExampleV2 {exampleV2TextField = "hello", exampleV2IntField = 1, exampleV2BoolField = True}
      let actual = eitherDecodeJSONViaCodec input :: Either String Lib.Example
      actual `shouldBe` Right expected

    it "fails parsing v1 on version mismatch" $ do
      let input = "{\"version\": 2, \"text\": null, \"int\": 1 }"
      let actual = eitherDecodeJSONViaCodec input :: Either String Lib.Example
      actual `shouldSatisfy` isLeft
