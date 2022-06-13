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
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Control.Lens (non, (&), (.~), (^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict.InsOrd qualified as HM
import Data.Maybe (fromJust)
import Data.OpenApi (HasName (name), HasSchema (schema), OpenApi)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Declare (undeclare)
import Data.OpenApi.Lens
  ( HasComponents (components),
    HasSchemas (schemas),
  )
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | An OpenApi document includes \"schemas\" that describe the data that may be
-- produced or consumed by an API. It can also include \"paths\" which describe
-- REST endpoints, and the document can include other API metadata. This example
-- only includes schemas.
openApi :: OpenApi
openApi = (mempty :: OpenApi) & components . schemas .~ HM.fromList (uncurry applySchemaName <$> openApiSchemas)

-- | As far as I can tell it is necessary to explicitly list all of the data
-- types that should be included in the OpenApi document with their names. It
-- would be nice to provide only a top-level type ('Example' in this case), and
-- have all of the types referenced by that type included automatically; but
-- I haven't seen a way to do that.
--
-- This list feeds into a hash map where the key must match the name given in
-- the codec implementation for the corresponding type (e.g. @\"ExampleV1\"@).
-- The helper 'applySchemaName' extracts the appropriate name to use for each
-- key, so it is only necessary to list keys here for schemas that do not have
-- a name defined in the codec.
openApiSchemas :: [(Maybe Text, OpenApi.NamedSchema)]
openApiSchemas =
  [ (Just "Example", toNamedSchema (Proxy :: Proxy Example)),
    (Nothing, toNamedSchema (Proxy :: Proxy ExampleV1)),
    (Nothing, toNamedSchema (Proxy :: Proxy ExampleV2)),
    (Nothing, toNamedSchema (Proxy :: Proxy ExampleV3))
  ]

-- | Introspect a given 'OpenApi.NamedSchema' to get its name, and return the
-- name with the unwrapped schema. (NamedSchema wraps a pair of an
-- 'OpenApi.Schema' and an optional name.)
--
-- Some named schemas don't have names. In that case provide the 'Just' value
-- from the first argument as the name.
--
-- Throws an exception if the named schema has no name, and no name is given via
-- the first argument.
applySchemaName :: Maybe Text -> OpenApi.NamedSchema -> (Text, OpenApi.Schema)
applySchemaName givenName givenSchema = (givenSchema ^. name . non (fromJust givenName), givenSchema ^. schema)

toNamedSchema :: HasCodec a => Proxy a -> OpenApi.NamedSchema
toNamedSchema proxy = undeclare $ declareNamedSchemaViaCodec proxy

-- ---

data Example = V1 ExampleV1 | V2 ExampleV2 | V3 ExampleV3
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec Example)

-- | Sum types translate to union types in OpenApi documents via
-- `disjointEitherCodec` (or via `eitherCodec` if the encoded JSON is not
-- necessarily disjoint). In this case we ensure that the encodings of
-- 'ExampleV1', 'ExampleV2', and 'ExampleV3' are disjoint by using
-- 'versionField' in the codec for each variant which injects a field into the
-- JSON representation that requires a specific number to parse successfully.
--
-- A codec that represents more than two variants of different types requires
-- nesting `disjointEitherCodec`. Thankfully in the generated OpenApi
-- documentation these three variants are flattened into a single @oneOf@ list
-- of allowed schemas.
instance HasCodec Example where
  codec =
    dimapCodec decode encode $
      disjointEitherCodec
        (codec :: JSONCodec ExampleV1)
        ( disjointEitherCodec
            (codec :: JSONCodec ExampleV2)
            (codec :: JSONCodec ExampleV3)
        )
        <?> "The format comes in multiple distinct versions"
    where
      decode = either V1 $ either V2 V3
      encode = \case
        V1 v1 -> Left v1
        V2 v2 -> Right $ Left v2
        V3 v3 -> Right $ Right v3

-- ---

data ExampleV1 = ExampleV1
  { exampleTextField :: Maybe Text,
    exampleIntField :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec ExampleV1)

instance HasCodec ExampleV1 where
  codec =
    object "ExampleV1" $
      ExampleV1
        -- A `version` field with the exact value @1@ is included in JSON
        -- output, and is required when parsing JSON into the 'ExampleV1' type,
        -- even though that field does not appear in the 'ExampleV1' data type.
        <$ versionField 1
        <*> requiredField "text" "text here if you want" .= exampleTextField
        <*> requiredField "int" "this should be a number" .= exampleIntField

-- ---

data ExampleV2 = ExampleV2
  { exampleV2TextField :: !Text,
    exampleV2IntField :: !Int,
    exampleV2BoolField :: !Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec ExampleV2)

instance HasCodec ExampleV2 where
  codec =
    object "ExampleV2" $
      ExampleV2
        <$ versionField 2
        <*> requiredField "text" "text is now non-nullable" .= exampleV2TextField
        <*> requiredField "int" "this should be a number" .= exampleV2IntField
        <*> requiredField "bool" "now there is a boolean" .= exampleV2BoolField

-- ---

data ExampleV3 = ExampleV3 {exampleV3Data :: [Text], exampleV3Param :: Int}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec ExampleV3)

instance HasCodec ExampleV3 where
  codec =
    object "ExampleV3" $
      ExampleV3
        <$ versionField 3
        <*> requiredField "data" "list of strings" .= exampleV3Data
        <*> requiredField "param" "some numeric option" .= exampleV3Param

-- ---

-- | Defines a required object field named @version@ that must have the given
-- integer value. On serialization the field will have the given value
-- automatically. On deserialization parsing will fail unless the field has the
-- exact given value.
versionField :: Integer -> ObjectCodec a Scientific
versionField v = requiredFieldWith' "version" (EqCodec n scientificCodec) .= const n
  where
    n = fromInteger v
