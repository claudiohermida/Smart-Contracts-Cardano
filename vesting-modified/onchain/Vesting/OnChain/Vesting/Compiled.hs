{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Vesting.OnChain.Vesting.Compiled
    ( vestingValidator
    ) where

-- import           PlutusCore.Version      (plcVersion100)
-- import           PlutusLedgerApi.V2      (PubKeyHash)
-- import           PlutusLedgerApi.V2
import qualified PlutusTx                  
-- import           GeniusYield.TxBuilder
-- import           GeniusYield.Types
import           PlutusTx.Prelude        (check) 
-- import           PlutusTx.Prelude            (Maybe(..),Integer,Bool, check, traceIfFalse, ($),
                                            --   (&&),(>=),(>),(<),
                                            --   return, fmap)
import           Vesting.OnChain.Vesting (mkVestingValidator)



-- Wrapping the typed validator to an untyped version
mkUntypedVestingValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkUntypedVestingValidator d r ctx =
    check (mkVestingValidator (PlutusTx.unsafeFromBuiltinData d) (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData ctx))

-- Compile the untyped validator to a Plutus Validator
-- vestingValidator :: Validator
vestingValidator :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
vestingValidator = $$(PlutusTx.compile [|| mkUntypedVestingValidator ||])


