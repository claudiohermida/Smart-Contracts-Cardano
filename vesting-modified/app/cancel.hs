import           Control.Monad         (unless)
import           GeniusYield.GYConfig
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           System.Environment    (getArgs)
import           Text.Printf           (printf)
import           Vesting.Api
import           Vesting.Utils         (findCollateral)

-- | Getting path for our core configuration and the beneficiary.
parseArgs :: IO (FilePath, FilePath)
parseArgs = do
    args <- getArgs
    case args of
        [coreCfgFile, skeyFile] -> return (coreCfgFile, skeyFile)
        _invalidArgument        -> fail "Error: wrong arguments, needed the configuration file and the beneficiary skey file\n"

main :: IO ()
main = do
    (coreCfgFile, skeyFile) <- parseArgs
    printf "configuration file: %s\nbenefactor skey file: %s\n" coreCfgFile skeyFile
    coreCfg <- coreConfigIO coreCfgFile
    skey    <- readPaymentSigningKey skeyFile
    let nid             = cfgNetworkId coreCfg
        benefactorPkh  = pubKeyHash $ paymentVerificationKey skey
        benefactorAddr = addressFromPubKeyHash nid benefactorPkh
    withCfgProviders coreCfg "claim-vesting" $ \providers -> do
        addr <- runGYTxQueryMonadNode nid providers  vestingAddress
        printf "vesting address: %s\n" $ addressToBech32 addr
        collateral <- findCollateral nid providers benefactorAddr

        vs <- runGYTxQueryMonadNode nid providers $ availableVestingsFromBenefactor benefactorPkh
        printf "found %d available vesting(s)\n" $ length vs
        unless (null vs) $ do
                    let ((oref,_):_) = vs
                    txBody <- runGYTxMonadNode nid providers [benefactorAddr] benefactorAddr collateral $ cancelVestingBenefactor nid oref
                    tid    <- gySubmitTx providers $ signGYTxBody txBody [skey]
                    printf "submitted tx: %s\n" tid
