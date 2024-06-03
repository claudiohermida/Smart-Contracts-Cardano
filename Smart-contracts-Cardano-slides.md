---
theme: gaia
_class: lead
paginate: true
backgroundColor: #fff
backgroundImage: images/Cardano-RGB_Logo-Stack-Blue.png
marp: true
---

![bg left:40% 80%](images/Cardano-RGB_Logo-Stack-Blue.png)

# **Smart Contracts in Cardano**

Claudio Hermida
<claudio.hermida@gmail.com>  
<https://www.linkedin.com/in/claudiohermida/>

---

# Synopsis

- State in functional programming ~~~> eUTXO model
- Specification of contract methods ~~~~> Validator
- What is a smart contract in Cardano?

---

``` java
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract VestingContract {
    address public beneficiary;
    address public benefactor;
    uint public amount;
    uint public deadline;
    bool public consumed;

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



    function claim() public {
        require(msg.sender == beneficiary, "Only the beneficiary can call this function.");
        require(block.timestamp >= deadline, "Deadline has not passed yet.");
        require(!consumed, "Funds have already been released.");
        consumed = true;
        payable(beneficiary).transfer(amount);
    }

    function cancel() public {
        require(msg.sender == benefactor, "Only the benefactor can call this function.");
        require(block.timestamp < deadline, "Deadline has already passed.");
        require(!consumed, "Funds have already been released.");
        consumed = true;
        payable(benefactor).transfer(amount);
    }
}
```

---

## Account-based blockchain model

<div style="text-align: center;">

```
Account-Ledger = Map Address Value
```

```
smart contract = object ~~~> (state{balance, ...}, methods)
```

```
tx-method : Account-Ledger -> Account-Ledger
```

</div>

---

## Functional Programming

- **Stateless**: functions are evaluated, reading inputs and producing outputs without manipulating any state or storage variables.

- **Referentially transparent**: statalessness enables *equational reasoning*, allowing to replace equals by equals anywhere.

---

### Handling state

Imperative programming: a **variable** is an *object* with two methods: `read`  and `update`

To emulate such state in a stateless fashion, we must emulate these two operations.

- `read`: we must provide the contents of the variable as an additional *input*
- `update`:  we must produce the updated content as an additional *output* so that it can be used in later computation.

---

``` haskell
 reverse :: List A -> List A

 reverse l =
     let
        var accumulator:: List A
        reverseWithAccumulator :: List A -> List A
            reverseWithAccumulator EmptyList = accumulator -- read state
            reverseWithAccumulator a:l       = 
                    accumulator :=  a: accumulator;  -- update state
                    reverseWithAccumulator l
    in  
        accumulator := EmptyList;  -- initialize state
        reverseWithAccumulator l
```

---

``` haskell
 newType State = List A

 reverse l = 
 let 
    reverseWithAccumulator' :: (List A,State) -> (List A, State)

    reverseWithAccumulator'(EmptyList,acc)  = (acc,acc) 
                        -- read state => no change on second component
    reverseWithAccumulator' (a:l, acc) = 
                                reverseWithAccumulator' (l, a:acc) 
                        -- update state => modify second component                 
                
    in  
        fst $ reverseWithAccumulator (l,EmptyList) 
        -- supply initial state, 
        -- select first component to get result 
```

---

- **functional transactions** should have additional input/output to account for state
- state is associated to an **address**
- state split **(value, storage)**

<div style="text-align: center; font-size: 30px; margin-top: 20px;">
  &#x220E; the additional input/output representing state is an <em>eutxo</em>
</div>

| **eutxo =** |                                              |
|:------------|:---------------------------------------------|
| **address** | target address where the utxo is held/locked |
| **value**   | value locked in this utxo                    |
| **datum**   | local state                                  |

---

## EUTXO blockchain model

<div style="text-align: center;">

```
eUTXO-Ledger = {eutxo}
```

```
tx:  inputs |-> outputs

tx-effect : eUTXO-Ledger -> eUTXO-Ledger

tx-effect (ledger) = (legder \ inputs) + outputs 
```

</div>

---

## Sharded state

eutxos in Cardano are ***resources***: once *produced* by a transaction can be *consumed* or *spent* once, *and only once*, by another transaction (as inputs).

Cardano allows multiple etuxos to be held at a single address, thereby effectively splitting or *sharding* the local state at an address.

---
Immediate consequences of the resource nature of eutxos and sharding:

- efficiency: a transaction can select very precisely those utxos which hold the piece of state to consume, and transactions consuming different utxos can be computed/validated in parallel.
- *non-interference*: an eutxo can only be used by a single transaction, so different transactions cannot operate over the same piece of state  
==>  *predictability* of the outcome of transactions

---
## Transactions

Most ***radical difference*** between the two models

In account-based model : transactions **excuted on-chain**

In Cardano

<br>
<div style="border: 1px solid #999; padding: 10px; background-color: lightblue;"><strong>a transaction is computed off-chain and validated on-chain</strong>
</div>

---

## Validators

For on-chain validation, Cardano introduces

``` haskell
validator :: Datum -> Redeemer -> ScriptContext -> Bool
```
to validate/allow the consumption of an *input* in a transaction. 
- **Datum**: is the datum of the input eutxo
- **Redeemer**: is the action/method whose execution we are validating
- **ScriptContext**: indicates *purpose* of the script being executed (spending an eutxo, minting, staking, etc) and the *transaction context* (inputs, outputs, signatures).

---
### Input eutxo

- eutxo held at a ***PublicKeyHash address***: unlocks with *private key signature*
- eutxo held at a ***script address***: unlocks by evaluating successfully the validator associated with the address:
    `script address ~ hash(validator code)`

---
### Separation of building and execution of tx:

- **tx build**: specify its inputs (eutxos) and its would-be outputs (eutxos). Here, eutxos are treated as mere *data*, not resources, since we are off chain. We also indicate the signer of the transaction, which pays for the *transaction fees* (supplied as one of the inputs eutxo). In order to specify outputs, some computation might take place from the given inputs (method execution); this is the *off-chain (pre)computation* of a transaction. We produce a so-called *transaction body*,

---
- **tx execute**
  - we *sign and submit* the transaction body for *validation*: each input held at a script triggers the execution of the corresponding validator and each input held at a pkh is signature-verified (*on-chain*)


  - if all inputs validate and the transaction satisfies preservation-of-values (that is, inputs and outputs are balanced), the transaction submission is *successful* and the ledger gets updated: inputs are spent (eliminated) and outputs are created (added) (***data ~~> resources***) Otherwise, the transaction *fails* and has no effect on the ledger.

---
| **Account based**                                                                                                                                                                                       |
|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| \- Transaction signed and submitted to a node                                                                                                                                                           |
| \- Changes to the ledger effected **on-chain** by means of state updates of the accounts involved in the transaction                                                                                    |
| \- Protected execution **on-chain**: if anything fails, it *reverts* to initial state of the ledger before transaction execution started, using a snapshot of the state taken at the start of execution |
| \- **Validation** mixed with **execution**, by means of **require** statements                                                                                                                          |
| \- Access to accounts during execution could be unrestricted (Ethereum) or restricted to pre-specified ones at transaction-build time (Algorand)                                                        |

---

| **eUTXO based**                                                                                                                   |
|:----------------------------------------------------------------------------------------------------------------------------------|
| \- Transaction signed and submitted to a node                                                                                     |
| \- Outputs of a transaction **precomputed off-chain;**                                                                            |
| \- (Inputs, Outputs) fed as transaction context to cardano-node at transaction-build time                                         |
| \- (Inputs, outputs) are subject to **validation** in the node (**on-chain**) before effecting a change on the distributed ledger |

---
# Part II
## Specification in the small: pre- and postconditions



---
How do we specify a sorting function?
`sortIncOrder: List(Int) -> List(Int)`

- Output must be increasingly ordered:
  ```
    Ordered:: List(Int) -> Bool
    Ordered [] = True
    Ordered [n] = True
    Ordered n:(m:l) = n \leq m && Ordered(m:l)

    forall l:List(Int). Ordered(sortIncOrder(l))

---
How do we specify a sorting function?
`sortIncOrder: List(Int) -> List(Int)`

- Output must be increasingly ordered:

  ```

    forall l:List(Int). Ordered(sortIncOrder(l))
    ```
- output should be a permutation or rearrangement of input
```
    isPermutation:: List(Int) -> List(Int) -> Bool

    isPermutation []    []  = True
    isPermutation (h:t) l = (h `elem` l) && isPermutation t (l \ h)

    forall l: List(Int). isPermutation(l,sortIncOrder(l))
```

---

How do we specify a sorting function?
`sortIncOrder: List(Int) -> List(Int)`

- Output must be increasingly ordered:

```

    forall l:List(Int). Ordered(sortIncOrder(l))
```
- output should be a permutation or rearrangement of input

```

    forall l: List(Int). isPermutation(l,sortIncOrder(l))
```

```
Spec(sortIncOrder)= forall l: List(Int).Ordered(sortIncOrder(l)) && isPermutation(l,sortIncOrder(l))
```

---
In general, for a function `f: I -> O`, we want to specify its ***extensional behaviour***: given input `i:I` satisfying a ***precondition*** `Pre(i)` its output `fi:O` should satisfy the ***postcondition*** `Post(i,fi)`:

```
forall i:I. Pre(i) => Post(i,fi)
```
What happens when `Pre(i)` does not hold? In the context of blockchains, we want that if the precondition for a method is not satisfied, its specification fails: `!Pre(i) => false`

```
Spec(f) = forall i:I. (Pre(i) => Post(i,fi)) && (!Pre(i) => false)
        = forall i:I. Pre(i) && Post(i,fi)
```

---
### Example (from Aiken library)
`from_asset_list(xs: List<(PolicyId, List<(AssetName, Int)>)> -> Value`

  Promote an arbitrary list of assets into a Value. This function fails if
- there’s any duplicate amongst PolicyId; `nodup $ map fst xs`
- there’s any duplicate amongst AssetName; `all $ map (nodup $ map fst) xs`
- the AssetName aren’t sorted in ascending lexicographic order; `all (Ordered map fst)`
- or any asset quantity is null. `none $ concat (map (map (\(assetName,qty) -> qty == 0)))`

---

  This function is meant to turn arbitrary user-defined Data into safe Value, while checking for internal invariants. The result should be the addition of the values represented by each individual entry `(PolicyId,AssetName,Int)`, which we write as `SingletonValue`. So, the ***Postcondition*** is:

  `from_asset_list (xs) == foldr <> mempty $ concat (map (\pid -> map (\(an,qty) -> SingletonValue(pid,an,qty))) xs`

---

### Validators and contract methods

How are we to use validators in the context of implementing smart contracts? The answer is:

***A validator specifies the pre- and post-condition of a contract method***
---
---
```javascript
function claim() public {
        require(msg.sender == beneficiary, "Only the beneficiary can call this function.");
        require(block.timestamp >= deadline, "Deadline has not passed yet.");
        require(!consumed, "Funds have already been released.");
        consumed = true;
        payable(beneficiary).transfer(amount);
    }
```

```javascript
Pre = (msg.sender == beneficiary)    -- signed by beneficiary
    && (block.timestamp >= deadline) -- deadline reached
    && [!consumed]                   -- amount not withdrawn yet
    && (address(this).balance >= amount) -- enough funds to transfer
```
```javascript
Post =
 (address(this).balance()_after == address(this).balance()_before - amount ) && 
 (beneficiary.balance()_after == beneficiary.balance()_before + amount)
```
---

``` haskell
data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , benefactor: PubKeyHash
    , deadline    :: POSIXTime
    , amount: Integer
    }

data VestingRedeemer = Claim | Cancel

mkVestingValidator :: VestingDatum -> VestingRedeemer -> ScriptContext -> Bool
mkVestingValidator dat Claim ctx =
    let
    -- PRECONDITIONS
        signedByBeneficiary = txSignedBy info $ beneficiary dat
        deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info
    -- POSTCONDITION
        amountPaidToBeneficiary = ( (getSignerAdaAmount info beneficiary) == amount dat)

    in
        traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
        traceIfFalse "deadline not reached" deadlineReached &&
        traceIfFalse "wrong amount paid" amountPaidToBeneficiary 
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

  -- extract the integer denominator (lovelace) of the Ada value of the outputs sent to a given address:
    getSignerAdaAmount :: TxInfo -> PubKeyHash ->Integer
    getSignerAdaAmount info addr = valueOf (valuePaidTo info addr) adaSymbol adaToken
```
---

- In the account-based model, since the contract methods operate on the whole state of the contract, we must use a boolean flag `consumed` to indicate whether the portion of the contract balance which is vested has already been consumed or not. This effectively turns that portion of the balance (state) into a *resource managed by the flag*: it is turned off when created and turned on when consumed or spent. In the validators, the resource-nature of eutxos makes this flag unnecessary: an input eutxo is either available or not, the node validating a transaction checks that for us.

---

- Conversely, we might notice that in the account-based model, the transfer of `amount` to `beneficiary/benefactor` is achieved by invoking a system-defined method `payable(beneficiary).transfer(amount)` which has its own (prespecified) pre and postconditions.
- The validator, on the other hand, cannot know whether the funds unlocked will satisfy the transfer postcondition; we only know that the value in the input will be consumed, but we must ensure that it reaches the beneficiary/benefactor, hence the additional check `amountPaidToBeneficiary`



---
## Transaction Schema

We call **transaction schema** any function that produces a transaction body `TxBody` as output, that is

***Transaction Schema = parameterized Transaction Body***
---
---

<img src="images/Claim-diagram.png" alt="Claim diagram" style="width: 70%; height: auto;">

---
### Building Transaction schema

We use Atlas (<https://atlas-app.io/>) to maintain the functional programming theme, but there are several alternative frameworks for transaction building.


- modular building of transactions via *skeletons*: add an input, an output, a validity range, a signatory, etc. ~~>`GYTxSkeleton` monad with a monoid structure. 
  
- skeleton passed to a *transaction builder* (`gyTxBuilder` in `GYTxMonadNode`), along with relevant parameters for interacting with a node: *network id*, *data providers*, etc.

--- 

-  transaction builder will attempt to complete the transaction body using a coin selection algorithm to select relevant fee and collateral input eutxos, and well as balancing the transaction by sending unspent input value to a change address.

---
```haskell
-- implement business logic of Claim method
claimVestingBeneficiary :: GYTxQueryMonad m => GYPubKeyHash -> GYTxOutRef -> m (GYTxSkeleton 'PlutusV2)
claimVestingBeneficiary beneficiary oref  = do
    slot <- currentSlot
    return $ isInvalidBefore slot <>                        -- sets up the validity interval
             mustBeSignedBy beneficiary <>                  -- adds a required signatory
             mustHaveInput GYTxIn                           -- adds input
                { gyTxInTxOutRef = oref
                , gyTxInWitness  = GYTxInWitnessScript      -- specify parameters to consume input eutxo:
                    (GYInScript $ vestingValidatorScript)   -- script
                    Nothing                                 -- inline datum
                    Claim                                   -- redeemer
                } <>
             mustHaveOutput $ GYTxOut                        -- adds output
                { gyTxOutAddress = beneficiary               -- target address
                , gyTxOutValue   = getValueFromTxOutRef oref -- amount to transfer
                , gyTxOutDatum   = Nothing                   -- no datum
                , gyTxOutRefS    = Nothing                   -- no reference script
                }

-- Helper function to get the GYValue from a GYTxInTxOutRef
getValueFromTxOutRef :: GYTxQueryMonad m => GYTxInTxOutRef -> m (Maybe GYValue)
getValueFromTxOutRef txOutRef = do
    -- Query the UTxO
    utxo <- gyQueryUtxoAtTxOutRef txOutRef
    -- Extract the GYValue
    return $ fmap gyTxOutValue utxo
```
---
```haskell
-- build transaction to Claim vested amount:

    txBody <- runGYTxMonadNode networkId providers beneficiary collateral $ 
        claimVestingBeneficiary beneficiary oref
    tid     <- gySubmitTx providers $ signTx txBody [skey]
```
---

## Smart Contracts in Cardano
 ***Cardano smart contract = 
set of transaction schemas and associated validators***
- schemas and validators must ‘share’  `Datum` and `Redeemer`
- `Datum`: *product* of components of local state of the contract (besides its `Value`):
<div style="border: 1px solid #999; padding: 10px;
font-size: 1.5rem; background-color: lightgray;">
data Datum = Datum { var<sub>1</sub> :: Type<sub>1</sub> , var<sub>2</sub> :: Type<sub>2</sub> , … , var<sub>n</sub> :: Type<sub>n</sub> }
</div>

- `Redeemer`: *sum* type;  alternatives <~~> methods signatures:

<div style="border: 1px solid #999; padding: 10px;
font-size: 1.5rem; background-color: lightgray;">
data Redeemer = method<sub>1</sub> Type<sub>11</sub> … Type<sub>1p</sub> | … | method<sub>m</sub> Type<sub>m1</sub> … Type<sub>mq</sub>
</div>

---

The correspondence of “methods” to redeemer cases need not be 1-1: some methods interact with various ‘pieces of state’ which will be represented by several inputs, each to be consumed by a possibly different redeemer. And conversely, a complex transaction involving several inputs implements a business logic that may correspond to a combination of several “methods” in the OOP sense.

---

## Deploying contracts

In account-based models, a smart contract is an ***object*** or ***instance*** of a class.

- ***Deploying*** a contract amounts to *creating* a new instance of the class and assigning it to an account address, which will hold the *local state* of that object/instance.
- The initial state is set upon deployment invoking the *constructor* method in the contract.
- one may deploy *several* instances of the same contract class, each residing at a different account address.

---
### Deploying Cardano smart contracts

1. Generate a ***script address*** that is going to hold the local state of the contract via eutxos. It is generated by hashing the Plutus validator script.
2. Build and submit a transaction with an output (eutxo) trageted at the script address, encoding the *intial state* of the instance in its *Value* and *Datum*. This  is  analogous to executing the *constructor* method of the contract. 

---

``` haskell
-- set up initial state to deploy Vesting contract
placeVesting :: GYTxQueryMonad m => GYPubKeyHash -> GYPubKeyHash -> GYTime -> GYValue -> m (GYTxSkeleton 'PlutusV2)
placeVesting benefactor beneficiary deadline valueAmount = 
    return $ mustHaveOutput $ GYTxOut       --specify an eutxo with inline datum
        { gyTxOutAddress = vestingAddress
        , gyTxOutValue   = valueAmount
         , gyTxOutDatum   = Just (datumFromPlutusData $ 
                              VestingDatum benefactor beneficiary 
                                           {timeToPlutus deadline) (toLovelace $ fromValue valueAmount)
                            , GYTxOutUseInlineDatum)
        , gyTxOutRefS    = Nothing
        }
```

``` haskell
-- build initial transaction to deploy contract:

    txBody <- runGYTxMonadNode networkId providers sender collateral $ 
        placeVesting
            benefactor beneficiary (timeFromPlutus deadline) 
            (valueFromLovelace $ toInteger amount)
    tid    <- gySubmitTx providers $ signTx txBody [skey]
```
---

### Declaration of a contract
In the acccount-based model *declaring* a contract amounts to storing its code *on-chain* so it can be referred to by other contracts as a library, as well as avoiding storing the contract code in each instance. 

In Cardano, we build an eutxo with the validator ***reference script*** (CIP-33), so that the ‘contract class’ is on-chain and we can refer to it for execution.

The reason for putting the code in an eutxo is that the very simple model of ledger in Cardano implies that if we want to associate any information to an address, we must build an eutxo targetted at that address, containing that info.

---
### Contract instances

In order to accommodate the possibility of several instances of the same contract coexising in the blockchain, we *mint an NFT* which is incorporated in the value of the initializing eutxo and passed along as we update contract state via new eutxos. 

Such NFT uniquely identifies the instance; it is the actual counterpart of the *account address* of the contract object:

---

| Account based            | eUTXO Based                                                 |
|--------------------------|-------------------------------------------------------------|
| contract (class)         | validator script + transaction schema |
| contract (object)        | initial-state-eutxo \@ script address                       |
| contract account address | contract token (NFT)                                        |
| declaration              | reference script eutxo                                      |

---

## The State Monad

$$\big[ \mathsf{(A \times B) \longrightarrow C} \big] \Longleftrightarrow \big[\mathsf{A \longrightarrow (B \longrightarrow C)}\big]$$
```haskell
    curry :: ((A,B) -> C) -> (A -> (B -> C))
    curry f = \a -> (\b -> f (a,b))

    uncurry:: (A -> (B -> C)) -> ((A,B) -> C)
    uncurry g = \(a,b) -> g a b
```
which set up an *isomorphism*
```
curry $ uncurry g == g              uncurry $ curry f == f
```
---
```
 f:: (I,S) -> (O,S)  ~~~~~> curry(f):: I -> <u>(S -> (O,S))</u>
 ```
*State monad* at the type `O`:
```haskell
    State:: * -> *
    State O :: S -> (O,S)
```

<strong><em>A function</em></strong> $\mathsf{f : I \longrightarrow O}$ <strong><em>with state</em></strong> $\mathsf{S}$ <strong><em>corresponds to a monadic computation</em></strong> $\mathsf{curry(f) : I \longrightarrow State\  O}$</em></strong>
