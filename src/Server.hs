{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Servant
import API.Prove (ProveAPI)
import Handlers.ProveHandler (proveHandler)
import Types.Instances ()  -- Import the instances

type API = ProveAPI

server :: Server API
server = proveHandler

app :: Application
app = serve (Proxy :: Proxy API) server
