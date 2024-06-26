{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Prover.API.Types.ZkProof where


import           Control.DeepSeq                          (NFData)
import           Data.ByteString                          (ByteString)
import           GHC.Generics                             (Generic)
import           Prelude

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof(..))

--TODO: implement `Binary` for our Plonk-related types to get rid of this mock type
data ZkProof = ZkProof
  deriving (Show, Eq, Generic, NFData)

instance NonInteractiveProof ZkProof where
  type Transcript ZkProof = ByteString
  type SetupProve ZkProof = ByteString
  type SetupVerify ZkProof = ByteString
  type Witness ZkProof = ByteString
  type Input ZkProof = ByteString
  type Proof ZkProof = ByteString

  setupProve _ = ""
  setupVerify _ = ""
  prove _ _ = ("", "")
  verify _ _ _ = True