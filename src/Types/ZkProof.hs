{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Types.ZkProof where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (empty)
import GHC.Generics (Generic)
import ZkFold.Base.Protocol.NonInteractiveProof

data ZkProof = ZkProof
  deriving (Show, Eq, Generic, NFData)

instance NonInteractiveProof ZkProof where
  type Transcript ZkProof = ByteString
  type SetupProve ZkProof = ByteString
  type SetupVerify ZkProof = ByteString
  type Witness ZkProof = ByteString
  type Input ZkProof = ByteString
  type Proof ZkProof = ByteString

  setupProve _ = BS.empty
  setupVerify _ = BS.empty
  prove _ _ = (BS.empty, BS.empty)
  verify _ _ _ = True
