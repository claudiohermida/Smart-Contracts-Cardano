## Smart contracts in Cardano

> The purpose of this note is to

>> - Show how the eUTxO model of blockchain comes about from a consideration of handling state in functional programming
>> - Exhibit Cardano's validators as verifiable specifications of a contract-method input-output behaviour
>> - Provide a putative answer to the question: what is a smart contract in Cardano?

For ease of presentation, we organize the material in three parts (posted on Medium https://medium.com/@claudio.hermida and this github repo https://github.com/claudiohermida/Smart-Contracts-Cardano):

>>   - Part I: Functional Programming and the eUTXO Model  https://medium.com/@claudio.hermida/smart-contracts-in-cardano-2ab2fc7de1aa
>    We analyse state in the account-based model of smart contracts and reinterpret it in the functional context, obtaining eutxo data
>>   - Part II: Specifications and Validators https://medium.com/@claudio.hermida/smart-contracts-in-cardano-6ebc3993db0f
>    Starting from an account-based sample smart contract, we illustrate the connection between the formal specification of its methods and validators and offchain code in Cardano (using Plutus/Atlas).
>>   - Part III: Transaction Schemas https://medium.com/@claudio.hermida/smart-contracts-in-cardano-65a521f26a7c
>    Completing the offchain part of the contract, we postulate the notion of transaction-body-valued (transaction schema) as the equivalent of contract-method calls, and argue about the use of NFT to indicate contract-instances.

This Youtube video presents the whole material (courtesy of Gimbalabs Playground) 
https://www.youtube.com/watch?v=cUu-7FDV0wI%3Fsi%3DcSD8uurYBGDYs7vL
