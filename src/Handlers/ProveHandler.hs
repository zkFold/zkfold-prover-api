{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.ProveHandler where

import Servant
import Data.ByteString (ByteString)
import ZkFold.Base.Protocol.NonInteractiveProof (proveAPI, ProveAPIResult(..))
import ZkFold.Base.Data.ByteString (fromByteString)
import Data.Maybe (fromJust)
import Types.ZkProof (ZkProof)
import Types.Instances ()

proveHandler :: ByteString -> ByteString -> Handler ProveAPIResult
proveHandler bsS bsW = do
  let setup = fromJust (fromByteString bsS)
      witness = fromJust (fromByteString bsW)
  return $ proveAPI @ZkProof setup witness
