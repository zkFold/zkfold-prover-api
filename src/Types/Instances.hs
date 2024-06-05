{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Instances where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, withObject, (.:), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Aeson.Key as Key
import ZkFold.Base.Protocol.NonInteractiveProof (ProveAPIResult(..))

-- Define ToJSON and FromJSON instances for ByteString
instance ToJSON BS.ByteString where
  toJSON = Aeson.String . T.pack . BS.unpack . B64.encode

instance FromJSON BS.ByteString where
  parseJSON = Aeson.withText "ByteString" $ \t ->
    case B64.decode (BS.pack (T.unpack t)) of
      Left err -> fail err
      Right bs -> return bs

-- Define ToJSON and FromJSON instances for ProveAPIResult
instance ToJSON ProveAPIResult where
  toJSON (ProveAPISuccess bs) = 
    object [Key.fromString "status" .= ("success" :: String), Key.fromString "data" .= bs]
  toJSON ProveAPIErrorSetup = 
    object [Key.fromString "status" .= ("error" :: String), Key.fromString "message" .= ("Setup error" :: String)]
  toJSON ProveAPIErrorWitness = 
    object [Key.fromString "status" .= ("error" :: String), Key.fromString "message" .= ("Witness error" :: String)]

instance FromJSON ProveAPIResult where
  parseJSON = withObject "ProveAPIResult" $ \v -> do
    status <- v .: Key.fromString "status"
    case (status :: String) of
      "success" -> do
        dataStr <- v .: Key.fromString "data"
        return $ ProveAPISuccess dataStr
      "error" -> do
        message <- v .: Key.fromString "message"
        case (message :: String) of
          "Setup error" -> return ProveAPIErrorSetup
          "Witness error" -> return ProveAPIErrorWitness
          _ -> fail "Unknown error message"
      _ -> fail "Unknown status"
