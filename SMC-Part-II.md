<div class="container-fluid main-container">

# Smart Contracts in Cardano

## Part II: Specifications and Validators

**Claudio Hermida**  
<claudio.hermida@gmail.com>  
<https://www.linkedin.com/in/claudiohermida/>

## Abstract

> The purpose of this work is to

> > -   Show how the eUTxO model of blockchain comes about from a
> >     consideration of handling state in functional programming
> > -   Exhibit Cardano’s validators as verifiable specifications of a
> >     contract-method input-output behaviour
> > -   Provide a putative answer to the question: what is a smart
> >     contract in Cardano?
>
> This is the second in a three-part series covering these topics. The
> other parts are <a href=https://github.com/claudiohermida/Smart-Contracts-Cardano/blob/main/SMC-Part-I.md>Part I</a> and <a href=https://github.com/claudiohermida/Smart-Contracts-Cardano/blob/main/SMC-Part-III.md>Part III</a>. The whole article is presented in this YouTube video
>
>
> <https://www.youtube.com/watch?v=cUu-7FDV0wI?si=cSD8uurYBGDYs7vL>
-   [Introduction](#introduction)
-   [Specification in the small: methods, pre- and
    post-conditions](#specification-in-the-small-methods-pre--and-post-conditions)
-   [Specifying a transaction:
    validators](#specifying-a-transaction-validators)
-   [Validators and contract methods](#validators-and-contract-methods)
-   [References](#references)

# Introduction

In the first part <a href=https://github.com/claudiohermida/Smart-Contracts-Cardano/blob/main/SMC-Part-I.md>Part I</a> of this series we recalled how functional
programming handles state and its relevance to understand Cardano’s
eutxo model.

In this second part, we focus on the more original aspect of Cardano’s
approach to smart contract, namely *transaction validation*, making
emphasis on Cardano’s radical difference wrt the account-based model,
namely, off-chain computation of transactions and their on-chain
validation. We thus are led to write out the whole specification that a
transaction must satisfy, namely the conjunction of validators for its
inputs plus the *preservation of values* balancing condition.

We recall the traditional approach to specifying the extensional
behaviour of programs (that is, their input-output behaviour) by means
of (first-order logic) *pre* and *post*-conditions, which we treat as
boolean-valued functions in our programming language, leading to their
use as *validators* for the corresponding *redeemer* method. We
illustrate these notions with our guiding example of a Solidity smart
contract implementing a *Vesting* escrow.

The third part <a href=https://github.com/claudiohermida/Smart-Contracts-Cardano/blob/main/SMC-Part-III.md>Part III</a> of this series addresses the relation of
smart contract methods and their off-chain counterpart via transaction
schemas, and rounds up our treatment of smart contracts, including their
deployment.

<div id="transactions" class="section level2">

# Transactions

In the account-based model, a transaction transforms the global state of
the blockchain. A transaction executes a *contract call*, whereby we
invoke a method of a contract with specific arguments and the current
state of the blockchain (meaning all the accounts’ local states indexed
by their addresses) as inputs, producing an updated global state as
output. This execution is carried *on-chain* by all the nodes involved
in processing of the transaction for its eventual inclusion in a block.

The most radical difference of Cardano’s eUTXO model is its treatment of
transactions: in Cardano

> **a transaction is computed off-chain and validated on-chain**

On-chain validation is the key feature of the Cardano blockchain. A
**validator** is a predicate which will indicate whether an input eutxo
can be consumed by the transaction; it has the following signature:

    validator :: Datum -> Redeemer -> ScriptContext -> Bool

Here `Datum` is the datum in the input eutxo we are attempting to
consume, `Redeemer` is the action/method whose execution we are
validating and `ScriptContext` indicates both the purpose of the script
to be executed (spending an input, minting tokens, staking, etc.) and
the transaction context (its inputs, outputs, and signatures).

Recall that we are operating under the functional-programming paradigm,
so the only *actual* input that we provide to a validator is the
redeemer, which is the equivalent of a *method signature* (name and
arguments). Datum is a piece of *local state* encoded in the input
eutxo, while the script context is the piece of *global state* the
transaction has access to.

Presently, there are two major kinds of input
eutxos<span id="Note1"><sup>[(CIP-31)](#CIP31)</sup></span>:

1.  eutxos held at a PublicKeyHash (pkh) address: these are the values
    held at an addreess controlled by a private key, which is required
    in the signature to unlock/spend such eutxo.
2.  eutxos held at a *script address*: a script address is uniquely
    associated to a validator (via hashing) and it is controlled by it.
    In order to unlock such eutxo, a node evaluates the validator (with
    a supplied redeemer); if it succeeds, the eutxo may be spent.

If all inputs of a transaction validate successfully (either by
validator or signature), such inputs are consumed and its outputs are
added to the ledger. Otherwise, the trasaction *fails* and no changes
occur in the ledger.

Let us elaborate the process of *building* and *executing* a
transaction:

-   we *build* a transaction by specifying its inputs (eutxos) and its
    would-be outputs (eutxos). Here, eutxos are treated as mere *data*,
    not resources, since we are off chain. We also indicate the signer
    of the transaction, which pays for the *transaction fees* (supplied
    as one of the inputs eutxo). In order to specify outputs, some
    computation might take place from the given inputs (method
    execution); this is the off-chain (pre)computation of a transaction.
    We produce a so-called *transaction body*,

-   we *sign and submit* the transaction body for *validation*: each
    input held at a script triggers the execution of the corresponding
    validator and each input held at a pkh is signature-verified.

-   if all inputs validate and the transaction satisfies
    preservation-of-values (that is, inputs and outputs are balanced),
    the transaction submission is *successful* and the ledger gets
    updated: inputs are spent (eliminated) and outputs are created
    (added): the triples of `(address,value,datum)` of our would-be
    outputs get uniquely referenced by the transaction-id we have just
    validated, together with the respective index, becoming genuine
    *eutxos*.

If any of the inputs fails to validate or the transaction is not
properly balanced, the transaction *fails* and has no effect on the
ledger.

The eUTXO model lends itself to a very simple modelling<sup>(Brünjes and
Gabbay 2020)</sup> of a distributed ledger: it is the set or collection
of eutxos

     Ledger = { eutxo } 

The blockchain itself is nothing more than a sequence of transactions
(grouped in blocks), such that the inputs of one transaction are outputs
of transactions occurring earlier in the sequence.

    Blockchain = [block]
    block = {tx}

There is no structure associating utxos to a given address; that
information has to be collected by inspecting the blockchain.

A transaction consumes its inputs and produces new outputs:

    tx :  Ledger  ~~~~~>   ( Ledger \ inputs ) ∪ outputs

Let us compare the salient aspects of transactions on both models:

<table>
<colgroup>
<col style="width: 100%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>Account based</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">- Transaction signed and submitted to a
node</td>
</tr>
<tr class="even">
<td style="text-align: left;">- Changes to the ledger effected
<strong>on-chain</strong> by means of state updates of the accounts
involved in the transaction</td>
</tr>
<tr class="odd">
<td style="text-align: left;">- Protected execution
<strong>on-chain</strong>: if anything fails, it <em>reverts</em> to
initial state of the ledger before transaction execution started, using
a snapshot of the state taken at the start of execution</td>
</tr>
<tr class="even">
<td style="text-align: left;">- <strong>Validation</strong> mixed with
<strong>execution</strong>, by means of <strong>require</strong>
statements</td>
</tr>
<tr class="odd">
<td style="text-align: left;">- Access to accounts during execution
could be unrestricted (Ethereum) or restricted to pre-specified ones at
transaction-build time (Algorand)</td>
</tr>
</tbody>
</table>

<table>
<colgroup>
<col style="width: 100%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>eUTXO based</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">- Transaction signed and submitted to a
node</td>
</tr>
<tr class="even">
<td style="text-align: left;">- Outputs of a transaction
<strong>precomputed off-chain;</strong></td>
</tr>
<tr class="odd">
<td style="text-align: left;">- (Inputs, Outputs) fed as transaction
context to cardano-node at transaction-build time</td>
</tr>
<tr class="even">
<td style="text-align: left;">- (Inputs, outputs) are subject to
<strong>validation</strong> in the node (<strong>on-chain</strong>)
before effecting a change on the distributed ledger</td>
</tr>
</tbody>
</table>

### <span id="CIP31">Reference input</span>

Since the Vasil hardfork, CIP-31 <span class="citation">(Peyton Jones
2021b)</span> was implemented allowing for a new kind of input:
**reference input**. It allows transactions to simply treat some inputs
as *read-only data* rather than *resources*: we can read their value and
datum, but not modify them. That way we avoid the waste of compute in
consuming and (re)producing a given utxo just to read its content.
[↩︎](#Note1)

# Specification in the small: methods, pre- and post-conditions

Consider a sorting function which orders a list of integers in
increasing order:

`sortIncOrder: List(Int) -> List(Int)`

Beyond our intuitive understanding which we can illustrate with examples
`sortIncOrder [8,2,5] == [2,5,8]`, how do we formally specify such a
function? For starters, we want the output increasingly ordered

    Ordered:: List(Int) -> Bool
    Ordered [] = True
    Ordered [n] = True
    Ordered n:(m:l) = n \leq m && Ordered(m:l)

    forall l:List(Int). Ordered(sortIncOrder(l))

That is not enough, otherwise simply returning the empty list would do.
We also expect the output list to a be a rearrengement or *permutation*
of the input list

    isPermutation:: List(Int) -> List(Int) -> Bool

    isPermutation []    []  = True

    isPermutation (h:t) l = (h `elem` l) && isPermutation t (l \ h)

    forall l: List(Int). isPermutation(l,sortIncOrder(l))

Now we have a sound *specification* of what a sorting function should
do, regardless of its implementation (quick sort, bubble sort, etc.)

Consider a functional program (or a procedural program modelled as a
function)

`f:I -> O`

Given an input `i: I` it produces (if it terminates) an output `fi: O`.
In order to *specify* its expected (extensional) behaviour, we must
indicate a couple of predicates, called *precondition* and
*postcondition*. The *precondition* constraints what kind of inputs
should the function expect, while the postcondition indicates how inputs
and outputs should relate. The specification states &gt; *for every
input that satisfies the precondition, it and the result satisfy the
postcondition*

which is expressed in first-order logic as

> ∀*i* : *I* .  Pre(*i*) ⇒ Post(*i*, *f**i*)

### Examples

-   For `sortIncOrder` we have no precondition as it should work for all
    lists, so we get

<!-- -->

        Spec(sortIncOrder) = forall l:List(Int). isSorted(sortIncOrder(l)) && isPermutation(l,sortIncOrder(l))

-   As another example consider the following piece from the ***Aiken
    stdlib***
    <https://aiken-lang.github.io/stdlib/aiken/transaction/value.html>
    (we elaborate on Values later
    on<span id="noteValues"><sup>[Values](#Values)</sup></span>):

    `from_asset_list(xs: List<(PolicyId, List<(AssetName, Int)>)>) -> Value`

    Promote an arbitrary list of assets into a Value. This function
    fails if (***Precondition***):

    -   there’s any duplicate amongst PolicyId; `nodup $ map fst xs`
    -   there’s any duplicate amongst AssetName;
        `all $ map (nodup $ map fst) xs`
    -   the AssetName aren’t sorted in ascending lexicographic order;
        `all (Ordered map fst)`
    -   or any asset quantity is null.
        `none $ concat (map (map (\(assetName,qty) -> qty == 0)))`

    This function is meant to turn arbitrary user-defined Data into safe
    Value, while checking for internal invariants. The result should be
    the addition of the values represented by each individual entry
    `(PolicyId,AssetName,Int)`, which we write as `SingletonValue`. So,
    the ***Postcondition*** is:

    `from_asset_list (xs) == foldr <> mempty $ concat (map (\pid -> map (\(an,qty) -> SingletonValue(pid,an,qty))) xs`

The specification above is a *partial specification*, as it does not
constrain the program behaviour when the precondition does not hold.
However, in blockchains, we do know that when the precondition of a
method call does not hold, the execution should revert; in other words,
if the precondition does not hold, the specification is falsified. In
summary, we want

    Spec(f) =
        forall i:I. Pre(i) ==> Post(i,fi) &&  !Pre(i) ==> false

This is equivalent to the more concise

    Spec(f) = forall i:I. Pre(i) && Post(i,fi)

<div id="specifying-a-transaction-validators" class="section level2">

## Specifying a transaction: validators

A transaction in Cardano is a function on **resource types**, which
consumes inputs and produces outputs. Just like data types specifiy
collections of elements on which one can perform certain operations
(like arithmetic operations on intergers or floats), resource types (or
*linear types*) specify collections of elements which, in addition to
the operations we can perform on them, are **produced** and
**consumed**. Once produced, they can be used ***only once***
(consumed).

Remember that in Cardano, `LEDGER = {eutxo}` and a transaction in
Cardano is a function `tx: LEDGER → LEDGER` whose effect is

    tx(L) = (L - {inputs tx}) + {outputs tx}

with `inputs tx` being a set of utxos and `outputs tx` a list of utxos,
regarded as a set with their unique identifiers via their position in
the output list of the transaction.

A transaction <span style="color:blue">tx</span>
![](https://github.com/claudiohermida/Smart-Contracts-Cardano/blob/main/images/tx.png)
must satfisfy its ***specification***:

> ***SPEC(tx) = validator***<sub>1</sub> (datum(in<sub>1</sub>),
> redeemer<sub>1</sub>, sc<sub>1</sub>) ∧ … ∧
> validator<sub>n</sub>(datum(in<sub>n</sub>),redeemer<sub>n</sub>,
> sc<sub>n</sub>) ∧ Preservation-of-Value(sc)

where **sc**<sub>i</sub> is the script context associated to
**in**<sub>i</sub> to unlock it, and

> ***Preservation-of-Value(scriptContext)*** = “sum of values of inputs
> plus value minted = sum of values of outpus plus tx fee”:

> ***Value(in***<sub>1</sub>) + … + Value(in<sub>n</sub>) +
> Value(forge(tx)) = Value(out<sub>1</sub>) + … +
> Value(out<sub>m</sub>) + Fee(tx)

or, in Plutus notation:

value (in<sub>1</sub>) &lt;&gt; … &lt;&gt; value(in<sub>n</sub>)
&lt;&gt; txInfoMint == value’(out<sub>1</sub>) &lt;&gt; … &lt;&gt;
value’(out<sub>m</sub>) &lt;&gt; txInfoFee

where  
value :: TxInInfo -&gt; Value  
value in = txOutValue $ txInInfoResolved in

value’:: TxOut -&gt; Value  
value’ = txOutValue

### <span id="Values">Value</span>

In Cardano, the type **Value** amounts to a bag of assets

    Value = Bag(AssetClass)

    AssetClass = {unAssetClass :: (CurrencySymbol, TokenName)}

This is **not** the definition of Value in the Plutus repository, but
merely its abstract algebraic characterisation. The Value type is
implemented via nested maps (as outlined in our second
[example](#noteValues) , which shows additional constraints imposed on
such maps to be considered values).

A bag (also called a *multiset*) is a list where ‘order does not
matter’; all that matters is the number of occurrences or repetitions of
an element in the bag. Just like, for a given set A, List(A) is the
*free monoid* on A, Bag(A) is the *free commutative monoid* on A; it is
crucial for the above formula of preservation of value that the
operation &lt;&gt; be commutative as well as
associative.[↩︎](#noteValues)

<div id="validators-and-contract-methods" class="section level3">

### Validators and contract methods

How are we to use validators in the context of implementing smart
contracts? The answer is:

> ***A validator specifies the pre- and post-condition of a contract
> method***

Let us illustrate this with our running example of a vesting contract.
Consider the method `claim()`: we deduce from it a
`Validator (dat,Claim,ctx)` and a *transaction
schema*<sup>[](@transactionSchema)</sup> which gets instantiated into a
concrete transaction to submit to the blockchain once we fill in its
required inputs and compute its outputs:

![](https://github.com/claudiohermida/Smart-Contracts-Cardano/blob/main/images/Claim-diagram.png?raw=true)

Here is the Haskell code of the validator for the `Claim` method of our
smart contract, the one for `Cancel` is entirely analogous and does not
bear repeating in detail \[^full-haskell-vesting\]:

    data VestingDatum = VestingDatum
        { beneficiary :: PubKeyHash
        , benefactor  :: PubKeyHash
        , deadline    :: POSIXTime
        , amount      :: Integer
        }

    data VestingRedeemer = Claim | Cancel

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
      


    mkVestingValidator dat Cancel ctx =
        ...
          signedByCreator = txSignedBy info $ benefactor dat
        ...
           

A few differences are worthy of notice between the formulation of
requirements/preconditions in both models:

-   In the account-based model, since the contract methods operate on
    the whole state of the contract, we must use a boolean flag
    `consumed` to indicate whether the portion of the contract balance
    which is vested has already been consumed or not. This effectively
    turns that portion of the balance (state) into a *resource managed
    by the flag*: it is turned off when created and turned on when
    consumed or spent. In the validators, the resource-nature of eutxos
    makes this flag unnecessary: an input eutxo is either available or
    not, the node validating a transaction checks that for us.

-   Conversely, we might notice that in the account-based model, the
    transfer of `amount` to `beneficiary/benefactor` is achieved by
    invoking a system-defined method
    `payable(beneficiary).transfer(amount)` which has its own
    (prespecified) pre and postconditions: ***pre*** is simply that
    `this.balance() >= amount` (sufficient funds available) and
    ***post*** is

<!-- -->

     address(this).balance()_after == address(this).balance()_before - amount && 
     beneficiary.balance()_after == beneficiary.balance()_before + amount

Notice that this postcondition refers to the implicit *state* and we
actually have to reference the state `_before` and `_after` the
execution of `transfer`.

The validator, on the other hand, cannot know whether the funds unlocked
will satisfy the transfer postcondition; we only know that the value in
the input will be consumed, but we must ensure that it reaches the
beneficiary/benefactor, hence the additional check
`amountPaidToBeneficiary` or `amountPaidToBenefactor` respectively. A
peculiarity of Cardano transactions is that there must be one input
eutxo that pays for the transaction fee; the eutxo might not have the
precise amount required, so there is a *change* paid back to the signer,
hence the total received might be greater than the `amount` coming from
the vesting eutxo. Also notice the `validateInputAmount` precondition
which does *type check* whether the input eutxo we are about to consume
really respects the logic of our contract by locking exactly the
`amount` of ADA stipulated in the datum. This kind of check is necessary
as Cardano cannot guarantee/restrict which type of eutxos are targeted
at the contract address.

\[^full-haskell-vesting\]
<https://github.com/claudiohermida/Smart-Contracts-Cardano/vesting-modified/onchain/Vesting/OnChain/Vesting.hs>

## References

Brünjes, Lars, and Murdoch J Gabbay. 2020. “UTxO-Vs Account-Based Smart
Contract Blockchain Programming Paradigms.” In *Leveraging Applications
of Formal Methods, Verification and Validation: Applications: 9th
International Symposium on Leveraging Applications of Formal Methods,
ISoLA 2020, Rhodes, Greece, October 20–30, 2020, Proceedings, Part III
9*, 73–88. Springer.
