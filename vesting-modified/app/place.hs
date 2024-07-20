import           Data.Text             (pack)
import           GeniusYield.GYConfig
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           System.Environment    (getArgs)
import           Text.Printf           (printf)
import           Text.Read             (readMaybe)
import           Vesting.Api
import           Vesting.Utils         (findCollateral)

-- | Getting path for our core configuration and the beneficiary.
parseArgs :: IO (FilePath, FilePath, GYPubKeyHash, GYPubKeyHash, GYTime, Natural)
parseArgs = do
    args <- getArgs
    case args of
        [coreCfgFile, skeyFile, benefactor, beneficiary, deadline', lovelace'] -> do
            let mBenefactorPkh = do 
                    benefactorAddr <- addressFromTextMaybe $ pack benefactor
                    addressToPubKeyHash benefactorAddr
                mBenenficiaryPkh = do
                    beneficiaryAddr <- addressFromTextMaybe $ pack beneficiary
                    addressToPubKeyHash beneficiaryAddr
                mdeadline = gyIso8601ParseM deadline'
                mlovelace = readMaybe lovelace'
            case (mBenefactorPkh, mBenenficiaryPkh, mdeadline, mlovelace) of
                (Just benefactorPkh, Just beneficiaryPkh, Just deadline, Just lovelace) -> return (coreCfgFile, skeyFile, benefactorPkh, beneficiaryPkh, deadline, lovelace)
                (Nothing,_,_,_)                            -> fail "Error: benefactor address invalid\n"
                (_,Nothing, _, _)                          -> fail "Error: beneficiary address invalid\n"
                (_,_, Nothing,_)                           -> fail "Error: deadline invalid\n"
                (_,_, _, Nothing)                          -> fail "Error: invalid lovelace amount\n"
        _invalidArgument                                 -> fail
            "Error: wrong arguments, needed the configuration file, the sender skey file, the beneficiary address, the deadline and the amount\n"

main :: IO ()
main = do
    (coreCfgFile, skeyFile, benefactor, beneficiary, deadline, lovelace) <- parseArgs
    printf "configuration file: %s\nsender skey file: %s\nbenefactor: %s\nbeneficiary: %s\ndeadline: %s\namount: %d\n" coreCfgFile skeyFile benefactor beneficiary deadline lovelace
    coreCfg <- coreConfigIO coreCfgFile
    skey    <- readPaymentSigningKey skeyFile
    let nid    = cfgNetworkId coreCfg
        sender = addressFromPubKeyHash nid $ pubKeyHash $ paymentVerificationKey skey
    printf "sender: %s\n" sender
    withCfgProviders coreCfg "place-vesting" $ \providers -> do
        addr <- runGYTxQueryMonadNode nid providers vestingAddress
        printf "vesting address: %s\n" $ addressToBech32 addr
        collateral <- findCollateral nid providers sender
    --    printf "beneficiary collateral: %s\n" collateral

        txBody <- runGYTxMonadNode nid providers [sender] sender collateral $ placeVesting
                benefactor
                beneficiary
                deadline
                (valueFromLovelace $ toInteger lovelace)
        tid    <- gySubmitTx providers $ signGYTxBody txBody [skey]
        printf "submitted tx: %s\n" tid

