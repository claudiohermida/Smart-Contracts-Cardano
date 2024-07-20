{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


module Vesting.OnChain.Vesting
    ( VestingDatum(..)
    , VestingRedeemer(..)
    , mkVestingValidator
    , getAdaAmountFromValue
    ) where

import           PlutusLedgerApi.V1.Interval (contains, from, to)
import           PlutusLedgerApi.V2          (POSIXTime, 
                                              PubKeyHash,
                                              OutputDatum(..),
                                              Datum(..))
import           PlutusLedgerApi.V2.Contexts (ScriptContext (scriptContextTxInfo),
                                              TxInfo (txInfoValidRange),
                                              txSignedBy,
                                              TxOut,
                                              valuePaidTo,
                                              findDatum,
                                              txOutValue,
                                              findOwnInput,
                                              txInInfoResolved)
import           PlutusTx                    (BuiltinData,
                                              UnsafeFromData (unsafeFromBuiltinData),
                                              unstableMakeIsData,
                                              fromBuiltinData)
import           PlutusTx.Prelude            (Maybe(..),Integer,Bool, check, traceIfFalse, ($),
                                              (&&),(>=),(>),(<),
                                              return, fmap)
-- import Control.Monad.IO.Class (MonadIO, liftIO)                  
import GeniusYield.Types -- Import relevant GeniusYield types
import GeniusYield.TxBuilder
import PlutusTx.Trace (traceError) -- Import PlutusTx traceError function
import PlutusLedgerApi.V1.Value (Value,assetClassValueOf, valueOf, assetClass, adaSymbol, adaToken, AssetClass(..)) -- Import Plutus Ledger Value functions
-- import PlutusLedgerApi.V1 (TxInfo(..), TxOut(..)) -- Import Plutus Ledger API functions
                                              

data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , benefactor  :: PubKeyHash
    , deadline    :: POSIXTime
    , amount      :: Integer
    }

unstableMakeIsData ''VestingDatum



data VestingRedeemer = Claim | Cancel

unstableMakeIsData ''VestingRedeemer




{-# INLINABLE mkVestingValidator #-}

mkVestingValidator :: VestingDatum -> VestingRedeemer -> ScriptContext -> Bool
mkVestingValidator dat Claim ctx =
    let
        info = scriptContextTxInfo ctx
        amt = amount dat
        -- PRECONDITIONS
        signedByBeneficiary = txSignedBy info $ beneficiary dat
        deadlineReached = from (deadline dat) `contains`  txInfoValidRange info
        -- POSTCONDITION
        amountPaidToBeneficiary = getSignerAdaAmount info (beneficiary dat) >= amt
    in
        traceIfFalse "ill-formed input, amount and value mismatch" $ wellFormedInput ctx amt &&
        traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
        traceIfFalse "deadline not reached" deadlineReached &&
        traceIfFalse "insufficient funds" amountPaidToBeneficiary


mkVestingValidator dat Cancel ctx =
    let
        info = scriptContextTxInfo ctx
        amt = amount dat
	-- PRECONDITIONS
        signedByCreator = txSignedBy info $ benefactor dat
        deadlineNotReached = to (deadline dat) `contains` txInfoValidRange info
	-- POSTCONDITION
	amountPaidToBenefactor = ( getSignerAdaAmount info (benefactor dat) >= amt)
    in
        traceIfFalse "ill-formed input, amount and value mismatch" $ wellFormedInput ctx amt &&
        traceIfFalse "creator's signature missing" signedByCreator &&
        traceIfFalse "deadline reached" deadlineNotReached &&
	    traceIfFalse "wrong amount paid" amountPaidToBenefactor 
  
-- helper function: extract the integer denominator (lovelace) of the Ada value of the outputs sent to a given address:
{-# INLINABLE getSignerAdaAmount #-}
getSignerAdaAmount :: TxInfo -> PubKeyHash ->Integer
getSignerAdaAmount info addr = valueOf (valuePaidTo info addr) adaSymbol adaToken

-- | Extract the integer Ada amount from a UTXO value
{-# INLINABLE getAdaAmount #-}
getAdaAmount :: TxOut -> Integer
getAdaAmount txOut = getAdaAmountFromValue  $ txOutValue txOut 
  

getAdaAmountFromValue :: Value -> Integer
getAdaAmountFromValue value = assetClassValueOf value adaAssetClass
                                where
                                    adaAssetClass = assetClass adaSymbol adaToken

-- helper function to validate input utxo: Ada value >= amount
{-# INLINABLE wellFormedInput #-}
wellFormedInput:: ScriptContext -> Integer -> Bool
wellFormedInput ctx amount = 
    getAdaAmount ownInput >= amount
    where 
        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "vesting input missing"
            Just i  -> txInInfoResolved i



