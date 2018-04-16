# lambdacoin
Basic PoW CryptoCurrency for LambdaConf 2018

## Design Goals
1. Proof of Work Consensus
2. Asymmetric Cryptographic Signatures
3. P2PKH Transactions
4. P2P Discovery
5. P2P Bootstrap via Hardcoded address
6. Fixed Difficulty PoW
7. Pure Haskell Implementation
8. Only Cryptographic Dependencies

## History
Bitcoin was built out of the cypherpunk desire to have money on the internet that was not centrally controlled.
How do we build money? Money has 3 main use cases: Store of Value, Medium of Exchange and Unit of Account. It also
has certain desirable properties to help fulfill these main use cases. It should be Portable, Recognizeable, Durable,
Scarce, Divisible and Fungible.

## Implementation 1
1. Central Database
2. No Auth
3. Fixed Initial Money Supply 100% granted to creator

## Problems with Implementation 1
1. Fragile
2. Insecure
3. Distribution of Money is not ideal
4. Central power can reverse or make up transactions with impunity

## Implementation 2
