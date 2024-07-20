---
generator: pandoc
title: Functional-programming-and-eUTXO.knit
viewport: width=device-width, initial-scale=1
---

<div class="container-fluid main-container">

<div id="header">

</div>

<div style="text-align: center; font-family: Arial, sans-serif;">

# Smart Contracts in Cardano

## Part III: Transaction Schemas
**Claudio Hermida**  
<claudio.hermida@gmail.com>  
<https://www.linkedin.com/in/claudiohermida/>

</div>

<div id="abstract" class="section level2">

## Abstract

> The purpose of this note is to

> > - Show how the eUTxO model of blockchain comes about from a consideration of handling state in functional programming
> > - Exhibit Cardano’s validators as verifiable specifications of a contract-method input-output behaviour
> > - Provide a putative answer to the question: what is a smart contract in Cardano?

This is third and final part of the three-part series covering these topics. The
other parts are (SMC-PartI) and (SMC-PartII). The whole article is presented in this YouTube video
>
><https://www.youtube.com/watch?v=cUu-7FDV0wI?si=cSD8uurYBGDYs7vL>

- [Introduction](#introduction)
- [Building Tx schemas](#building-tx-schemas)
- [Smart Contracts in Cardano](#smart-contracts-in-cardano-1)
    - [Deploying contracts](#deploying-contracts)
   
- [References](#references)

# Introduction

In the first part (SMC-PartI) of this series we dealt with state in functional programming and its relevance to Cardano's eutxo model, while in the second part (SMC-PartII) we set up the correspondence of formal specification of methods via pre and postconditions with (on-chain) validators for the corresponding redeemers.

In this final installment, we deal with the off-chain part of smart contract methods, namely the construction of transactions. To that end, we introduce the notion of *transaction schema* as a parameterized transaction body, and illustrate it with our guiding *Vesting* contract example. 

We conclude by analyzing what it means to *deploy* a smart contract in Cardano, and consider the use of NFTs in this context.



<div id="building-tx-schema" class="section level2">

# Building Tx schemas

Having set up validators for our methods, we must produce transactions to execute them. There are various frameworks for transaction building: MeshJS (<https://meshjs.dev/>) caters to JavaScript enthusiasts, offering React integration, while Atlas (<https://atlas-app.io/>) favors the more Cardano-native Haskell environment. In keeping with our functional programming emphasis we will use this latter as reference, although the concepts we expose are framework agnostic.

One attractive feature of Atlas is that it provides a modular way of building transactions, piece by piece so to speak: add an input, an output, a validity range, a signatory, etc. All these pieces are called *skeletons*: they live in a `GYTxSkeleton` monad with a monoid structure to combine such pieces via `<>`. A skeleton is fed to a *transaction builder*, like `gyTxBuilder` in `GYTxMonadNode`, along with relevant parameters for interacting with a node, such as network id and data providers. Such a transaction builder will attempt to complete the transaction body using a coin selection algorithm to select relevant fee and collateral input eutxos, and well as balancing the transaction by sending unspent input value to a change address.

<span id="transactionSchema">We call **transaction schema** any function that produces a transaction body `TxBody` as output, that is

> ***Transaction Schema = parameterized Transaction Body***
>
> So a composition of transaction builder with transaction-skeleton constructors would form such a schema. In the Atlas framework, a transaction body has type `GYTxBody`.

Let us see the relevant transaction schemas for our example Vesting contract. First, let us recall the original formulation of the `Claim` method in Solidity

``` java
    function claim() public {
        require(msg.sender == beneficiary, "Only the beneficiary can call this function.");
        require(block.timestamp >= deadline, "Deadline has not passed yet.");
        require(!consumed, "Funds have already been released.");
        consumed = true;
        payable(beneficiary).transfer(amount);
    }
```





For the formulation in Atlas/Haskell, we use *inline datums*<sup><span class="citation">[Peyton Jones 2021a](#ref-cip-32)</span></sup>:

```haskell
-- implement business logic of Claim method

claimVestingBeneficiary :: GYTxQueryMonad m => GYTxInTxOutRef -> m (GYTxSkeleton 'PlutusV2)
claimVestingBeneficiary oref  = do
    slot <- currentSlot
          
    utxo <- gyQueryUtxoAtTxOutRef txOutRef
    datum <- inlineDatum utxo
    return $ isInvalidBefore slot 
    beneficiary <- extractBeneficiaryFromDatum datum      -- gets beneficiary from inline datum     
    return $ isInvalidBefore slot <>                        -- sets up the validity interval
             mustBeSignedBy beneficiary <>                  -- adds a required signatory
             mustHaveInput GYTxIn                           -- adds input
                { gyTxInTxOutRef = oref
                , gyTxInWitness  = GYTxInWitnessScript      -- specify parameters to consume input eutxo: 
                    (GYInScript $ vestingValidatorScript)   -- script
                    datum                                 -- inline datum
                    Claim                                   -- redeemer
                } <>
             mustHaveOutput $ GYTxOut                        -- adds output
                { gyTxOutAddress = beneficiary               -- target address
                , gyTxOutValue   = getValueFromTxOutRef oref -- amount to transfer
                , gyTxOutDatum   = Nothing                  -- no datum
                , gyTxOutRefS    = Nothing                   -- no reference script
                }


-- Helper function to get the GYValue from a GYTxInTxOutRef
getValueFromTxOutRef :: GYTxQueryMonad m => GYTxInTxOutRef -> m (Maybe GYValue)
getValueFromTxOutRef txOutRef = do
    -- Query the UTxO
    utxo <- gyQueryUtxoAtTxOutRef txOutRef
    -- Extract the GYValue
    return $ fmap gyTxOutValue utxo

 -- Helper function to get GYPubKeyHash of beneficiary out of a GYTxInTxOutRef with inline datum
<!--
extractBeneficiary ::    GYTxQueryMonad m => GYTxInTxOutRef -> m (Maybe GYPubKeyHash)
 extractBeneficiary txOutRef = do
        utxo <- gyQueryUtxoAtTxOutRef txOutRef
        dat <- inlineDatum utxo
        return $ extractBeneficiaryFromDatum dat
-->

-- Function to extract the beneficiary from the given datum

extractBeneficiaryFromDatum :: GYDatum -> Maybe GYPubKeyHash
extractBeneficiaryFromDatum (GYDatum d) = do
    VestingDatum {..} <- fromBuiltinData d
    toGYPubKeyHash $ Just beneficiary

-- Function to extract inline datum from a utxo
inlineDatum :: GYUTxO -> Maybe GYDatum 
inlineDatum utxo@GYUTxO{utxoOutDatum} = case utxoOutDatum of 
                                    GYOutDatumInline gyDatum -> Just gyDatum
                                    _                        -> Nothing

-- implement business logic of Cancel method
cancelVestingBenefactor :: GYTxQueryMonad m => GYTxOutRef -> m (GYTxSkeleton 'PlutusV2)
cancelVesting  oref  = do
    slot <- currentSlot
    benefactor <- extractBenefactor oref                      -- gets benefactor from inline datum
    return $ isInvalidAfter slot <>                           -- sets up the validity interval
             mustBeSignedBy benefactor <>                     -- adds a required signatory
             mustHaveInput GYTxIn                             -- adds input ... similar to claimVestingBeneficiary
                { gyTxInTxOutRef = oref
                , gyTxInWitness  = GYTxInWitnessScript
                    (GYInScript $ vestingValidatorScript)
                    Nothing 
                    Cancel
                } <>
             mustHaveOutput $ GYTxOut                          -- adds output ... similar to claimVestingBeneficiary
                { gyTxOutAddress = benefactor
                , gyTxOutValue   = getValueFromTxOutRef oref
                , gyTxOutDatum   = Nothing
                , gyTxOutRefS    = Nothing
                }
        

-- Wrapping the typed validator to an untyped version
mkUntypedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntypedVestingValidator d r ctx =
    check (mkVestingValidator (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData ctx))

-- Compile the untyped validator to a Plutus Validator
vestingValidator :: Validator
vestingValidator = mkValidatorScript $$(PlutusTx.compile [|| mkUntypedVestingValidator ||])

-- Create a wrapped version to use within GeniusYield
vestingAddress :: GYAddress
vestingAddress = scriptAddress vestingValidator
```

The code is rather verbose as there are a number of helper functions to extract data nested into records with lists of etc etc. Finally, we have to build transaction bodies out of the skeletons, sign and submit. Here are the essential details (for the full-fledged version of a similar contract see <span class="citation">[Brujnes 2023b](#ref-brujnes2023vestingApp)</span>)

``` haskell
-- build transaction to Claim vested amount:
    beneficiary <- extractBeneficiary oref 
    txBody <- runGYTxMonadNode networkId providers beneficiary collateral $ 
        claimVestingBeneficiary  oref
    tid     <- gySubmitTx providers $ signTx txBody [skey]


```

</div>

<div id="smart-contracts-in-cardano" class="section level2">

## Smart Contracts in Cardano

We could sum up our considerations of Cardano smart contracts with the slogan

> > ***A Cardano smart contract is a set of transaction schemas and their associated validators***

We have already established how such a set of schemas and validators must ‘share’ the data types `Datum` and `Redeemer`, in formulations compatible with the off-chain and on-chain setups, which might involve different languages. Essentially, `Datum` is a *product* of the various components of local state of the contract (besides its `Value`):

<div style="border: 1px solid #999; padding: 10px; background-color: lightgray;">

data Datum = Datum { var<sub>1</sub> :: Type<sub>1</sub> , var<sub>2</sub> :: Type<sub>2</sub> , … , var<sub>n</sub> :: Type<sub>n</sub> }

</div>

while `Redeemer` is a *sum* type, whose various alternatives correspond essentially to the methods signatures:

<div style="border: 1px solid #999; padding: 10px; background-color: lightgray;">

data Redeemer = method<sub>1</sub> Type<sub>11</sub> … Type<sub>1p</sub> \| … \| method<sub>m</sub> Type<sub>m1</sub> … Type<sub>mq</sub>

</div>

The correspondence of “methods” to redeemer cases need not be 1-1: some methods interact with various ‘pieces of state’ which will be represented by several inputs, each to be consumed by a possibly different redeemer. And conversely, a complex transaction involving several inputs implements a business logic that may correspond to a combination of several “methods” in the OOP sense.

</div>

<div id="appendix-the-state-monad" class="section level2">

<div id="deploying-contracts" class="section level2">

### Deploying contracts

In account-based models, a smart contract is an ***object*** or ***instance*** of a class. Deploying a contract amounts to *creating* a new instance of the class and assigning it to an account address, which will hold the *local state* of that object/instance. The initial state is set upon deployment invoking the *constructor* method in the contract. We can actually deploy *several* instances of the same contract class, each residing at a different account address; such account addresses hold the respective local states of those instances.

The analogous operation of deploying a contract in Cardano involves two steps:

> 1. Generate a ***script address*** that is going to hold the local state of the contract via eutxos. It is generated by hashing the Plutus validator script.
> 2. Build and submit a transaction with an output (eutxo) targeted at the script address, encoding the *intial state* of the instance in its Value and Datum. This  is  analogous to executing the *constructor* method of the contract.


In our example Vesting contract, let us recall the original constructor in Solidity

``` java
contract VestingContract {
    ...
    constructor(
        address _beneficiary,
        uint _deadline
    ) payable {
        beneficiary = _beneficiary;
        benefactor = msg.sender;
        amount = msg.value;
        deadline = _deadline;
        consumed = false;
    }
    ...
}
```


Here is the relevant code in Atlas/Haskell (recall that we use *inline datum*):

```haskell
-- set up initial state to deploy Vesting contract
placeVesting :: GYTxQueryMonad m => GYPubKeyHash -> GYPubKeyHash -> GYTime -> GYValue -> m (GYTxSkeleton 'PlutusV2)
placeVesting benefactor beneficiary deadline valueAmount = 
    return $ mustHaveOutput $ GYTxOut       --specify an eutxo with inline datum
        { gyTxOutAddress = vestingAddress   -- script address from mKVestingValidator
        , gyTxOutValue   = valueAmount
         , gyTxOutDatum   = Just (datumFromPlutusData $ 
                              VestingDatum benefactor beneficiary 
                                           {timeToPlutus deadline) (toLovelace $ fromValue valueAmount)
                            , GYTxOutUseInlineDatum)
        , gyTxOutRefS    = Nothing
        }

-- build initial transaction to deploy contract:
...
    txBody <- runGYTxMonadNode networkId providers sender collateral $
        placeVesting
            benefactor beneficiary (timeFromPlutus deadline)
            (valueFromLovelace $ toInteger amount)
    tid    <- gySubmitTx providers $ signTx txBody [skey]

```

We may also emulate the **declaration** of the contract code, which is a transaction that loads the code by itself on-chain (considered as a library of methods), as follows:

> Build an eutxo with a ***reference script***<span id="refscript"><sup>[CIP-33](#CIP33)</sup></span>, so that the ‘contract class’ is on-chain and we can refer to it whenever it is needed for execution (validation).

The reason for putting the code in an eutxo is that the very simple model of ledger in Cardano, namely as a set or collection of utxos, implies that if we want to associate any information to an address, we must build a utxo targeted at that address, containing that info.

<div style="border: 1px solid #999; padding: 10px; background-color: lightgray;">

### Reference Script CIP-33

A reference-script utxo carries the code of the validator script in a special new field. It was also introduced by <span class="citation">[Peyton Jones 2021b](#ref-cip-33)</span> implemented in the Vasil hard-fork. It greatly simplifies the size of transactions which invoke this validator, as we can just refer to the given utxo rather than pass the whole piece of code each time we refer to it.[↩︎](#refscript)

</div>
<br>

In order to accommodate the possibility of several instances of the same contract coexising in the blockchain, we *mint an NFT* which is incorporated in the value of the initializing eutxo and passed along as we update contract state via new eutxos. This NFT uniquely identifies the instance; it is the actual counterpart of the *account address* of the contract object. For a good example of this use of NFTs see <span class="citation">[Brujnes 2023a](#ref-bruines2023oracle)</span>.

<div style="border: 1px solid #999; padding: 10px; background-color: lightgray;">

### On NFT threat tokens

[https://developers.cardano.org/docs/smart-contracts](https://developers.cardano.org/docs/smart-contracts/)

> *Contract instances:* *When you have contracts designed to run in multiple steps, the UTXO that represents the current state of a specific instance/invocation of that script is something you need to be able to keep track of.* *There is no standard for how to do this as of now, but one way to accomplish this is to be to create a minting-policy that only allows minting of thread token NFTs to the script’s address, and then use the NFTs as thread-tokens by having the validator script enforce such NFTs be moved with each transaction.*
</div>
<br>

Here is a brief summary of related concepts in both blockchain models:

| Account based            | eUTXO Based                                                 |
|--------------------------|-------------------------------------------------------------|
| contract (class)         | validator script + [transaction schema](#transactionSchema) |
| contract (object)        | initial-state-eutxo \@ script address                       |
| contract account address | contract token (NFT)                                        |
| declaration              | reference script eutxo                                      |

</div>

<div id="references" class="section level1 unnumbered">

## References

<div id="ref-cip-32" class="csl-entry">

Peyton Jones, Michael. 2021a. “Inline Datums.” <https://cips.cardano.org/cip/CIP-32/>.

</div>


<div id="ref-cip-33" class="csl-entry">

———. 2021b. “Reference Scripts.” <https://cips.cardano.org/cip/CIP-33/>.

<div id="ref-bruines2023oracle" class="csl-entry">

Brujnes, Lars. 2023a. “Stablecoin with Oracle.” <https://github.com/input-output-hk/plutus-pioneer-program/tree/fourth-iteration/code/Week09>.

</div>

<div id="ref-brujnes2023vestingApp" class="csl-entry">

———. 2023b. “Vesting App.” <https://github.com/brunjlar/atlas-examples/tree/main/vesting/app>.

</div>
