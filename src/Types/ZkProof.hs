{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Types.ZkProof where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import ZkFold.Base.Protocol.NonInteractiveProof
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (empty)

-- Define your proof type
data ZkProof = ZkProof
  deriving (Show, Eq, Generic, NFData)

instance NonInteractiveProof ZkProof where
  type Transcript ZkProof = ByteString
  type Setup ZkProof = ByteString
  type Witness ZkProof = ByteString
  type Input ZkProof = ByteString
  type Proof ZkProof = ByteString

  setup _ = BS.empty
  prove _ _ = (BS.empty, BS.empty)
  verify _ _ _ = True
