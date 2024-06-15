{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Types.ZkProof where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.ByteString (ByteString)

import Servant
import Servant.Swagger
import Servant.Swagger.UI
import Data.Swagger
import Control.Lens
import GHC.Generics
import Data.ByteString
import ZkFold.Base.Data.ByteString
import ZkFold.Base.Protocol.NonInteractiveProof
import Data.Maybe (fromJust)
import Types.Instances ()

data ZkProof = ZkProof
  deriving (Show, Eq, Generic, NFData)

instance NonInteractiveProof ZkProof where
  type Transcript ZkProof = ByteString
  type SetupProve ZkProof = ByteString
  type SetupVerify ZkProof = ByteString
  type Witness ZkProof = ByteString
  type Input ZkProof = ByteString
  type Proof ZkProof = ByteString

  setupProve _ = ""
  setupVerify _ = ""
  prove _ _ = ("", "")
  verify _ _ _ = True

newtype SetupBytes
  = SetupBytes ByteString
    deriving (Generic, MimeUnrender OctetStream)

instance ToSchema SetupBytes where
  declareNamedSchema _ = pure $ NamedSchema (Just "Setup bytes") byteSchema

newtype WitnessBytes
  = WitnessBytes ByteString
    deriving (Generic, MimeUnrender OctetStream)

instance ToSchema WitnessBytes where
  declareNamedSchema _ = pure $ NamedSchema (Just "Witness bytes") byteSchema
