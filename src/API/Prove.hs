{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Prove where

import Servant
import Data.ByteString (ByteString)
import ZkFold.Base.Protocol.NonInteractiveProof (ProveAPIResult)

type ProveAPI = "prove" :> ReqBody '[OctetStream] ByteString :> ReqBody '[OctetStream] ByteString :> Post '[JSON] ProveAPIResult
