{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server (app, Env(..), runK) where

import Types.ZkProof (ZkProof, WitnessBytes(..), SetupBytes(..))
import Types.Instances ()
import Control.Lens ((&), (.~), (?~))
import Data.Maybe (fromJust)
import Data.Swagger
import Servant
import Servant.Swagger (HasSwagger(..))
import Servant.Swagger.UI ()
import System.IO (stdout)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Katip as K
import Data.ByteString (ByteString)

import ZkFold.Base.Data.ByteString (fromByteString)
import ZkFold.Base.Protocol.NonInteractiveProof (proveAPI, ProveAPIResult)

type API = ProveAPI :<|> DocAPI

newtype Env = Env { logger :: K.LogEnv }

type ProveAPI = "prove"
  :> ReqBody '[OctetStream] SetupBytes
  :> ReqBody '[OctetStream] WitnessBytes
  :> Post '[JSON] ProveAPIResult

runK :: MonadIO m => K.LogEnv -> K.LogStr -> m ()
runK le mes = K.runKatipContextT le () "loop" $ K.logLocM K.InfoS mes

proveHandler :: Env -> SetupBytes -> WitnessBytes -> Handler ProveAPIResult
proveHandler (Env env) (SetupBytes bsS) (WitnessBytes bsW) = do
  runK env "Start prove"
  let setup' = fromJust $ fromByteString @ByteString bsS
  let witness' = fromJust $ fromByteString @ByteString bsW
  let res = proveAPI @ZkProof setup' witness'
  runK env $ K.logStr $ "Setup: " ++ show setup'
  runK env $ K.logStr $ "Witness: " ++ show witness'
  runK env $ K.logStr $ "ProveAPi result: " ++ show res
  pure res

type DocAPI = "swagger.json" :> Get '[JSON] Swagger

docHandler :: Swagger
docHandler = toSwagger (Proxy @ProveAPI)
  & info.title .~ "Prover API Swagger reference"
  & info.version .~ "1.0"
  & info.description ?~ "..."
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

app :: Env -> Application
app env = serve (Proxy :: Proxy API) (proveHandler env :<|> pure docHandler)
