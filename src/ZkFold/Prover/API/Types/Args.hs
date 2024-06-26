module ZkFold.Prover.API.Types.Args where

import           Data.ByteString (ByteString)
import           Data.Swagger
import           GHC.Generics    (Generic)
import           Prelude
import           Servant         (MimeUnrender, OctetStream)

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
