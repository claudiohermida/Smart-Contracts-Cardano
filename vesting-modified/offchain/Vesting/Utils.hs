module Vesting.Utils
    ( findCollateral
    ) where

import           GeniusYield.TxBuilder
import           GeniusYield.Types     (GYAddress, GYNetworkId, GYProviders,
                                        GYTxOutRef)

findCollateral :: GYNetworkId -> GYProviders -> GYAddress -> IO ( Maybe (GYTxOutRef,Bool))
findCollateral nid providers addr = do
    m <- runGYTxQueryMonadNode nid providers $ getCollateral' addr 5_000_000
    case m of
        Just (oref, _) -> return $ Just (oref, True)
        Nothing        -> fail "Error: no collateral found\n"
