{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( openApi,
    Example (..),
    ExampleV1 (..),
    ExampleV2 (..),
  )
where

import Autodocodec
  ( Autodocodec (Autodocodec),
    Codec (EqCodec),
    HasCodec (codec),
    JSONCodec,
    ObjectCodec,
    dimapCodec,
    disjointEitherCodec,
    object,
    requiredField,
    requiredFieldWith',
    scientificCodec,
    (.=),
    (<?>),
  )
import Autodocodec.OpenAPI ()
import Control.Lens ((&), (.~))
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict.InsOrd qualified as HM
import Data.OpenApi (OpenApi)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Lens
  ( HasComponents (components),
    HasSchemas (schemas),
  )
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)

openApi :: OpenApi
openApi = (mempty :: OpenApi) & components . schemas .~ HM.fromList exampleSchemas

exampleSchemas :: [(Text, OpenApi.Schema)]
exampleSchemas =
  [ ("Example", OpenApi.toSchema (Proxy :: Proxy Example)),
    ("ExampleV1", OpenApi.toSchema (Proxy :: Proxy ExampleV1)),
    ( "ExampleV2",
      OpenApi.toSchema (Proxy :: Proxy ExampleV2)
    )
  ]

data Example = V1 ExampleV1 | V2 ExampleV2
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON,
      OpenApi.ToSchema
    )
    via (Autodocodec Example)

-- TODO: matchChoicesCodec
instance HasCodec Example where
  codec =
    dimapCodec decode encode $
      disjointEitherCodec
        (codec :: JSONCodec ExampleV1)
        (codec :: JSONCodec ExampleV2)
        <?> "The format comes in multiple distinct versions"
    where
      decode = either V1 V2
      encode = \case
        V1 v1 -> Left v1
        V2 v2 -> Right v2

data ExampleV1 = ExampleV1
  { exampleTextField :: Maybe Text,
    exampleIntField :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON,
      OpenApi.ToSchema
    )
    via (Autodocodec ExampleV1)

instance HasCodec ExampleV1 where
  codec =
    object "ExampleV1" $
      ExampleV1
        <$ versionField 1
        <*> requiredField "text" "text here if you want" .= exampleTextField
        <*> requiredField "int" "this should be a number" .= exampleIntField

data ExampleV2 = ExampleV2
  { exampleV2TextField :: !Text,
    exampleV2IntField :: !Int,
    exampleV2BoolField :: !Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON,
      OpenApi.ToSchema
    )
    via (Autodocodec ExampleV2)

instance HasCodec ExampleV2 where
  codec = object "ExampleV2" example
    where
      example =
        ExampleV2
          <$ versionField 2
          <*> requiredField "text" "text is now non-nullable" .= exampleV2TextField
          <*> requiredField "int" "this should be a number" .= exampleV2IntField
          <*> requiredField "bool" "now there is a boolean" .= exampleV2BoolField

versionField :: Integer -> ObjectCodec a Scientific
versionField v = requiredFieldWith' "version" (EqCodec n scientificCodec) .= const n
  where
    n = fromInteger v
