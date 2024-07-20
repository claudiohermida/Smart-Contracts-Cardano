-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}

module Vesting.Api
    ( vestingAddress
    , availableVestings
    , availableVestingsFromBenefactor
    , placeVesting
    , claimVestingBeneficiary
    , cancelVestingBenefactor
    ) where

import qualified Data.Map.Strict       as Map
import           GeniusYield.TxBuilder
import           GeniusYield.Types

import           Vesting.Script          (vestingValidatorGY,vestingAddress)
import           Vesting.OnChain.Vesting (VestingDatum(..),VestingRedeemer(..),getAdaAmountFromValue)

-- imports suggested by ChatGPT
import PlutusLedgerApi.V2 (fromBuiltinData)
import PlutusLedgerApi.V1.Value (assetClass)
import GHC.Generics()
import Cardano.Api()
import PlutusTx()
import Control.Monad (filterM)


{-
-- Corresponding off-chain types using GY types
data GYVestingDatum = GYVestingDatum
    { gyBeneficiary :: GYPubKeyHash
    , gyBenefactor  :: GYPubKeyHash
    , gyDeadline    :: GYTime
    , gyAmount      :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToData,FromData)

data GYVestingRedeemer = GYClaim | GYCancel
    deriving (Show, Generic, ToJSON, FromJSON, ToData, FromData)
-}

{-
-- Function to convert on-chain VestingDatum to GYVestingDatum
vestingDatumToGY :: VestingDatum -> GYVestingDatum
vestingDatumToGY (VestingDatum benf benr dl amnt) =
    GYVestingDatum (pubKeyHashToGY benf) (pubKeyHashToGY benr) (timeToPlutus dl) amnt

-- Function to handle VestingRedeemer in GY types
vestingRedeemerToGY :: VestingRedeemer -> GYVestingRedeemer
vestingRedeemerToGY Claim  = GYClaim
vestingRedeemerToGY Cancel = GYCancel
-}




{-
placeVesting :: GYTxQueryMonad m => GYPubKeyHash -> GYTime -> GYValue -> m (GYTxSkeleton 'PlutusV2)
placeVesting beneficiary deadline value = do
    addr <- vestingAddress
    return $ mustHaveOutput $ GYTxOut
        { gyTxOutAddress = addr
        , gyTxOutValue   = value
        , gyTxOutDatum   = Just (datumFromPlutusData $ timeToPlutus deadline, GYTxOutUseInlineDatum)
        , gyTxOutRefS    = Nothing
        }
-}

-- set up initial state to deploy Vesting contract
placeVesting :: GYTxQueryMonad m => GYPubKeyHash -> GYPubKeyHash -> GYTime -> GYValue -> m (GYTxSkeleton 'PlutusV2)
placeVesting benefactor beneficiary deadline valueAmount =  
            do addr <- vestingAddress
               return $ mustHaveOutput $ GYTxOut
                { gyTxOutAddress = addr
                , gyTxOutValue   = valueAmount
                , gyTxOutDatum   = Just (datumFromPlutusData $ VestingDatum (pubKeyHashToPlutus benefactor) (pubKeyHashToPlutus beneficiary) (timeToPlutus deadline) amt, GYTxOutUseInlineDatum)
                , gyTxOutRefS    = Nothing
                }
    where 
        amt = getAdaAmountFromValue $ valueToPlutus valueAmount


gyAdaAssetClass :: GYAssetClass 
gyAdaAssetClass = case assetClassFromPlutus (assetClass adaSymbol adaToken) of
                        Right gyac -> gyac
                        Left err -> error ("ill-formed asset class: " ++ show err) 


availableVestings :: GYTxQueryMonad m => GYPubKeyHash -> m [(GYTxOutRef, GYTime)]
availableVestings beneficiaryPkh = do
    slot   <- slotOfCurrentBlock
    now    <- slotToBeginTime slot
    addr   <- vestingAddress
    utxos  <- utxosAtAddress addr Nothing
    utxos' <- utxosDatums utxos
    orefDatums <- filterM (filterCondition now) (Map.toList utxos')
    return  [ (oref, timeFromPlutus $ deadline datum) | (oref, (_,_,datum)) <- orefDatums ]
        where 
            filterCondition gyTime (oref, (_,_,datum)) = do 
                                                    pkh <- pubKeyHashFromPlutus' $ beneficiary datum
                                                    return $ gyTime > timeFromPlutus (deadline datum) 
                                                          && pkh == beneficiaryPkh




availableVestingsFromBenefactor :: GYTxQueryMonad m => GYPubKeyHash -> m [(GYTxOutRef, GYTime)]
availableVestingsFromBenefactor benefactorPkh = do
    slot   <- slotOfCurrentBlock
    now    <- slotToBeginTime slot
    addr   <- vestingAddress
    utxos  <- utxosAtAddress addr Nothing
    utxos' <- utxosDatums utxos
    orefDatums <- filterM (filterCondition now) (Map.toList utxos')
    return  [ (oref, timeFromPlutus $ deadline datum) | (oref, (_,_,datum)) <- orefDatums ]
        where 
            filterCondition gyTime (oref, (_,_,datum)) = do 
                                                    pkh <- pubKeyHashFromPlutus' $ benefactor datum
                                                    return $ gyTime <= timeFromPlutus (deadline datum) 
                                                            && pkh == benefactorPkh






-- implement business logic of Claim method
claimVestingBeneficiary :: (GYTxQueryMonad m) =>  GYNetworkId -> GYTxOutRef -> m (GYTxSkeleton 'PlutusV2)
claimVestingBeneficiary nId oref  = do
    slot <- slotOfCurrentBlock
    mUtxomDatum <- utxoAtTxOutRefWithDatum oref
    case mUtxomDatum of 
        Just (_utxo ,Just datum) -> 
            case decodeDatum datum of 
                Just (VestingDatum _ beneficiary _ amt) -> 
                                        let rBeneficiaryPkh = pubKeyHashFromPlutus beneficiary
                                        in case rBeneficiaryPkh of 
                                            Right beneficiaryPkh ->
                                                 let addressBeneficiary = addressFromPubKeyHash nId beneficiaryPkh
                                                 in  return $ isInvalidBefore slot <>                    -- sets up the validity interval
                                                        mustBeSignedBy beneficiaryPkh <>              -- adds a required signatory
                                                        mustHaveInput GYTxIn                       -- adds input
                                                        { gyTxInTxOutRef = oref
                                                        , gyTxInWitness  = GYTxInWitnessScript
                                                            (GYInScript vestingValidatorGY)
                                                            (datumFromPlutusData datum)
                                                            (redeemerFromPlutusData Claim)
                                                        } <>
                                                        mustHaveOutput GYTxOut                   -- adds payment output
                                                        { gyTxOutAddress = addressBeneficiary
                                                        , gyTxOutValue   = valueFromLovelace $ toInteger amt
                                                        , gyTxOutDatum   = Nothing
                                                        , gyTxOutRefS    = Nothing
                                                        } 
                                            Left err -> 
                                                    error "unparseable beneficiary"
                Nothing -> error "ill-formed utxo"
        Nothing -> error "ill-formed utxo"
       
-- Decodes Datum to VestingDatum
decodeDatum :: GYDatum -> Maybe VestingDatum
decodeDatum d = fromBuiltinData $ datumToPlutus' d


-- implement business logic of Cancel method
cancelVestingBenefactor :: (GYTxQueryMonad m) =>   GYNetworkId -> GYTxOutRef -> m (GYTxSkeleton 'PlutusV2)
cancelVestingBenefactor nId oref  = do
    mUtxomDatum <- utxoAtTxOutRefWithDatum oref
    case mUtxomDatum of 
        Just (_utxo ,Just datum) -> 
                case decodeDatum datum of 
                    Just (VestingDatum benefactor _ deadline amt) -> do
                            deadlineSlot <- enclosingSlotFromTime' (timeFromPlutus deadline)
                            let rBenefactorPkh = pubKeyHashFromPlutus benefactor
                            case rBenefactorPkh of 
                                 Right benefactorPkh -> 
                                    let addressBenefactor = addressFromPubKeyHash nId benefactorPkh
                                    in   return $ isInvalidAfter deadlineSlot <>                         -- sets up the validity interval
                                            mustBeSignedBy benefactorPkh <>                   -- adds a required signatory
                                            mustHaveInput GYTxIn                           -- adds input
                                            { gyTxInTxOutRef = oref
                                            , gyTxInWitness  = GYTxInWitnessScript
                                                (GYInScript vestingValidatorGY)
                                                (datumFromPlutusData datum)
                                                (redeemerFromPlutusData Cancel)
                                            } <>
                                            mustHaveOutput  GYTxOut                       -- adds payment output
                                            { gyTxOutAddress = addressBenefactor
                                            , gyTxOutValue   = valueFromLovelace $ toInteger amt
                                            , gyTxOutDatum   = Nothing
                                            , gyTxOutRefS    = Nothing
                                            } 
                                 Left err -> error "unparseable benefactor"
                    Nothing -> error "ill-formed utxo"
        Nothing -> error "ill-formed utxo"

