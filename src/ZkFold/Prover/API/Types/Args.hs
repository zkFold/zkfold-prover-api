module ZkFold.Prover.API.Types.Args where

import           Control.DeepSeq             (NFData)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.ByteString             (ByteString)
import           Data.Swagger
import           GHC.Generics                (Generic)
import           Prelude
import           Servant                     (MimeUnrender, OctetStream)
import           ZkFold.Base.Data.ByteString
newtype SetupBytes
  = SetupBytes ByteString
    deriving (Generic, FromJSON)

instance ToSchema SetupBytes where
  declareNamedSchema _ = pure $ NamedSchema (Just "Setup bytes") byteSchema

newtype WitnessBytes
  = WitnessBytes ByteString
    deriving (Generic, FromJSON)

instance ToSchema WitnessBytes where
  declareNamedSchema _ = pure $ NamedSchema (Just "Witness bytes") byteSchema
