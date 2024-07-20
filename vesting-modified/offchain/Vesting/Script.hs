module Vesting.Script
    ( vestingValidatorGY,
      vestingAddress
    ) where

import           GeniusYield.Types
import           GeniusYield.TxBuilder
import qualified Vesting.OnChain.Vesting.Compiled as OnChain

vestingValidatorGY :: GYValidator 'PlutusV2
vestingValidatorGY = validatorFromPlutus OnChain.vestingValidator

-- Create the address for the validator script
vestingAddress :: GYTxQueryMonad m =>  m GYAddress
vestingAddress = scriptAddress  vestingValidatorGY