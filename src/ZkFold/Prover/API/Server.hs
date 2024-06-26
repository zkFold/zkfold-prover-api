{-# LANGUAGE TypeOperators #-}

module ZkFold.Prover.API.Server (Env(..), runK, app) where

import           Control.Lens                                    ((&), (.~), (?~))
import           Control.Monad.IO.Class                          (MonadIO(..))
import           Data.ByteString                                 (ByteString)
import           Data.Maybe                                      (fromJust)
import           Data.Swagger
import qualified Katip                                           as K
import           Prelude
import           Servant
import           Servant.Swagger                                 (HasSwagger(..))
import           Servant.Swagger.UI                              ()

import           ZkFold.Base.Data.ByteString                     (fromByteString)
import           ZkFold.Base.Protocol.NonInteractiveProof.Prover (ProveAPIResult, proveAPI)
import           ZkFold.Prover.API.Types.Args                    (SetupBytes (..), WitnessBytes (..))
import           ZkFold.Prover.API.Types.ZkProof                 (ZkProof)

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
