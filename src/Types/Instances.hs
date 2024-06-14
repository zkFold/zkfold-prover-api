{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Instances where

import Control.Lens hiding ((.=))
import GHC.Generics (Generic)
import Data.Swagger
import Data.Aeson
import Data.Text
import Data.Aeson.Types
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import ZkFold.Base.Protocol.NonInteractiveProof (ProveAPIResult(..), ProofBytes (..))

instance ToJSON ProveAPIResult where
  toJSON (ProveAPISuccess bs) = object
    [ "status" .= ("success" :: String)
    , "data" .= bs
    ]
  toJSON ProveAPIErrorSetup = object
    [ "status" .= ("error" :: String)
    , "message" .= ("Setup error" :: String)
    ]
  toJSON ProveAPIErrorWitness = object
    ["status" .= ("error" :: String)
    , "message" .= ("Witness error" :: String)
    ]

instance FromJSON ProveAPIResult where
  parseJSON = withObject "ProveAPIResult" $ \v ->
    v .: "status" & id @(Parser String) >>= \case
      "success" -> ProveAPISuccess <$> v .: "data"
      "error" -> v .: "message" & id @(Parser String) >>= \case
        "Setup error" -> return ProveAPIErrorSetup
        "Witness error" -> return ProveAPIErrorWitness
        _ -> fail "Unknown error message"
      _ -> fail "Unknown status"

instance ToSchema ProofBytes where
  declareNamedSchema _ = pure $ NamedSchema (Just "Proof bytes") byteSchema

instance ToSchema ProveAPIResult where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
