{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (app) where

import Types.ZkProof
import Types.Instances

import Control.Lens
import Data.Maybe
import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import ZkFold.Base.Data.ByteString
import ZkFold.Base.Protocol.NonInteractiveProof

type API = ProveAPI :<|> DocAPI

type ProveAPI = "prove"
  :> ReqBody '[OctetStream] SetupBytes
  :> ReqBody '[OctetStream] WitnessBytes
  :> Post '[JSON] ProveAPIResult

proveHandler :: SetupBytes -> WitnessBytes -> Handler ProveAPIResult
proveHandler (SetupBytes bsS) (WitnessBytes bsW) =
  pure $ proveAPI @ZkProof
    (fromJust $ fromByteString bsS)
    (fromJust $ fromByteString bsW)

type DocAPI = "swagger.json" :> Get '[JSON] Swagger

docHandler :: Swagger
docHandler = toSwagger (Proxy @ProveAPI)
  & info.title .~ "Prover API Swagger reference"
  & info.version .~ "1.0"
  & info.description ?~ "..."
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

app :: Application
app = serve (Proxy :: Proxy API) (proveHandler :<|> pure docHandler)
