{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module ZkFold.Prover.API.Types.ZkProof where

import           Data.Binary
import           Data.ByteString                                     (ByteString)
import           Data.Swagger                                        hiding
                                                                     (get, put)
import           GHC.Generics                                        (Par1,
                                                                      U1 (..),
                                                                      (:*:) (..))
import           GHC.TypeNats
import           Prelude                                             hiding
                                                                     (Bool,
                                                                      (==))
import           ZkFold.Base.Algebra.Basic.Class                     (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.Polynomials.Univariate
import           ZkFold.Base.Protocol.NonInteractiveProof.Internal
import           ZkFold.Base.Protocol.NonInteractiveProof.Prover     (ProofBytes (..),
                                                                      ProveAPIResult (..))
import           ZkFold.Base.Protocol.Plonk
import           ZkFold.Base.Protocol.Plonkup.Input
import           ZkFold.Base.Protocol.Plonkup.Proof
import           ZkFold.Base.Protocol.Plonkup.Prover
import           ZkFold.Base.Protocol.Plonkup.Utils
import           ZkFold.Base.Protocol.Plonkup.Witness
import           ZkFold.Prover.API.Types.Args                        (WitnessBytes (..))
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import           ZkFold.Symbolic.Data.Bool                           (Bool)
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FieldElement

instance
  forall f p i g1 .
  ( f ~ ScalarFieldOf g1
  , Binary (p f)
  , Binary (i f)
  ) => Binary (PlonkupWitnessInput p i g1) where
  put PlonkupWitnessInput{..} = put payloadInput <> put witnessInput
  get = PlonkupWitnessInput <$> get <*> get

instance (Binary (ScalarFieldOf g1))
  => Binary (PlonkupProverSecret g1) where
  put (PlonkupProverSecret a) = put a
  get = PlonkupProverSecret <$> get

instance (Binary (l (ScalarFieldOf g1)))
  => Binary (PlonkupInput l g1) where
  put = put . unPlonkupInput
  get = PlonkupInput <$> get

instance
  ( Binary g1
  , Binary (ScalarFieldOf g1)
  ) => Binary (PlonkupProof g1) where
  put PlonkupProof{..} = put cmA <> put cmB <> put cmC <> put cmF <> put cmH1 <> put cmH2 <> put cmZ1 <> put cmZ2 <> put cmQlow <> put cmQmid <> put cmQhigh <> put proof1 <> put proof2 <> put a_xi <> put b_xi <> put c_xi <> put s1_xi <> put s2_xi <> put f_xi <> put t_xi <> put t_xi' <> put z1_xi' <> put z2_xi' <> put h1_xi' <> put h2_xi <> put l1_xi
  get = PlonkupProof <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

instance ToSchema ProofBytes where
  declareNamedSchema _ = pure $ NamedSchema (Just "Proof bytes") byteSchema

instance ToSchema ProveAPIResult where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

type InputBytes = WitnessBytes

type PlonkExample n = Plonk (U1 :*: U1) (Par1 :*: U1) n Par1 BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point))

equalityCheckContract :: forall a c . (Symbolic c, FromConstant a (BaseField c)) => a -> FieldElement c -> Bool c
equalityCheckContract targetValue inputValue = inputValue == fromConstant targetValue

setupEqualityCheckContract :: SetupProve (PlonkExample 16)
setupEqualityCheckContract = setupProve plonk
  where
    ac = compile (equalityCheckContract @Fr 0) :: ArithmeticCircuit Fr (U1 :*: U1) (Par1 :*: U1) Par1
    (omega, k1, k2) = getParams 16
    x = fromConstant (5 :: Natural)
    (gs, h1) = getSecrectParams  x
    plonk = Plonk omega k1 k2 ac h1 gs :: PlonkExample 16
