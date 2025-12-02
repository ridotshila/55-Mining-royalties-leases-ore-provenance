# 🧾 Detailed Tutorial: Understanding and Using `Main.hs` (Mining Validator)

This tutorial explains your `Main.hs` module, which implements a **multi-purpose mining validator** for:

* **Royalty distribution**
* **Lease agreements with rent & collateral**
* **Ore batch NFT transfer & assay updates**

The validator dynamically handles **three different datums** and **six different actions**, making it a highly flexible on-chain script for mining operations.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Data Structures](#2-data-structures)
3. [🧠 Core Validator Logic](#3-core-validator-logic)
4. [🔢 Helper Functions](#4-helper-functions)
5. [⚙️ Script Wrapping & Compilation](#5-script-wrapping--compilation)
6. [💾 Writing the Validator File](#6-writing-the-validator-file)
7. [🧪 Practical Usage Example](#7-practical-usage-example)
8. [🧷 Testing Strategy](#8-testing-strategy)
9. [✅ Best Practices](#9-best-practices)
10. [📘 Glossary of Terms](#10-glossary-of-terms)

---

## 1. 📦 Imports Overview

### 🔹 Plutus Ledger Modules

Your validator uses:

* **Plutus.V2.Ledger.Api**
  Provides `ScriptContext`, `TxInfo`, `POSIXTime`, `TokenName`, `CurrencySymbol`, etc.

* **Plutus.V2.Ledger.Contexts**
  Supplies `txSignedBy` for signature checks.

* **Plutus.V1.Ledger.Interval**
  Used for validating time intervals via `contains` and `from`.

### 🔹 Serialization & File Output

* **Codec.Serialise** (Serializes the validator)
* **Cardano.Api / Shelley** (Writes `.plutus` files)

### 🔹 Other utilities

* **PlutusTx**
  Used to derive on-chain `IsData` instances.

* **PlutusTx.Prelude**
  Replaces standard Prelude inside on-chain code.

---

## 2. 🗃️ Data Structures

Your validator supports **three different datums**, each enabling a different contract mode.

### 🎧 2.1 RoyaltyEntry

Represents a single royalty payout rule:

* `reBenef` — beneficiary wallet (PubKeyHash)
* `reBps` — share in basis points (0–10000)

---

### 🎧 2.2 RoyaltySplitter

Defines a full royalty-splitting configuration:

* `rsOperator` — authorized settlement signer
* `rsEntries` — list of `RoyaltyEntry` records
* `rsPayoutCS` / `rsPayoutTN` — token used for payments

---

### 🏠 2.3 LeaseDatum

Represents a leasing contract:

* `ldLessor`, `ldLessee` — participants
* `ldRentAmount` — rent per payment
* `ldCollateralAmt` — locked collateral amount
* `ldCollateralCS`, `ldCollateralTN` — collateral token
* `ldLeaseEnd` — deadline for lease

---

### ⛏️ 2.4 OreBatchDatum

Used to track ore batches:

* `obOwner` — owner of NFT
* `obSource` — mine/location
* `obAssayHash` — latest assay or provenance
* `obNftCS`, `obNftTN` — identity of the ore batch NFT

---

### 🔧 2.5 Redeemer: MiningAction

The validator supports **six distinct actions**:

1. `DistributeRoyalties`
2. `PayRent`
3. `SeizeCollateral`
4. `EndLease`
5. `TransferBatch`
6. `AppendAssay`

Each action is strictly tied to one datum type.

---

## 3. 🧠 Core Validator Logic

The validator uses this core function:

```haskell
mkMiningValidator :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
```

It attempts three decoding branches:

### 3.1 Royalty Logic

If the datum is a `RoyaltySplitter`:

* Must be **signed by operator**
* Validity range must allow **settlementTime**
* Sum of BPS must be ≤ 10000
* All beneficiaries must receive correct token amounts

---

### 3.2 Lease Logic

If the datum is `LeaseDatum`:

Handles three possible redeemers:

#### **PayRent**

* Signed by **lessee**
* Timestamp must be valid
* Paid amount must ≥ rent
* Lessor must receive rent tokens

#### **SeizeCollateral**

* Signed by **lessor**
* Lease must have ended
* Collateral must be paid to lessor

#### **EndLease**

* Signed by lessor **or** lessee
* Timestamp must be valid

---

### 3.3 Ore Batch NFT Logic

If the datum is `OreBatchDatum`:

#### **TransferBatch**

* Signed by owner
* NFT must appear in outputs (ensures transfer)

#### **AppendAssay**

* Signed by owner
* New assay must be non-empty

---

## 4. 🔢 Helper Functions

Your script defines various important helpers:

### `pubKeyHashAddress`

Constructs a simple Plutus address.

### `valuePaidTo`

Gets the amount of a specific token paid to a given PubKeyHash.

### `nowInRange`

Checks whether the transaction’s time range includes a given timestamp.

### `sumBps`

Totals royalty basis points.

### `shareOf`

Calculates a beneficiary’s share of production.

### `allPaid`

Verifies all royalty payments are satisfied.

### `nftPresentInOutputs`

Ensures an NFT exists in transaction outputs.

---

## 5. ⚙️ Script Wrapping & Compilation

Plutus scripts must expose:

```
BuiltinData -> BuiltinData -> BuiltinData -> ()
```

You provide this via:

```haskell
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
```

Then compile with:

```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrapped ||])
```

---

## 6. 💾 Writing the Validator File

The script is written as:

```
mining-validator.plutus
```

The function:

```haskell
saveValidator :: IO ()
```

* Serializes validator
* Converts to `PlutusScriptV2` format
* Writes it to disk

---

## 7. 🧪 Practical Usage Example

In `ghci`:

```haskell
-- Compile and save the validator
main

-- Or manually
saveValidator
```

The output:

```
Mining validator written to: mining-validator.plutus
```

You can now push it to Cardano for testing.

---

## 8. 🧷 Testing Strategy

Recommended test scenarios:

### 🔹 Royalty tests

* Correct operator signature
* Incorrect BPS sum
* Missing beneficiary payment
* Wrong payout token

### 🔹 Lease tests

* Valid and invalid rent payments
* Early/late collateral seizure
* End lease signature combinations

### 🔹 Ore Batch tests

* NFT missing from outputs
* Invalid owner signatures
* Empty assay hash submissions

---

## 9. ✅ Best Practices

* Ensure datums are formed correctly before submission
* Always verify required signatures exist
* Test every branch separately (multi-datum scripts are tricky!)
* Provide clear trace messages (you already did great here)
* Validate token-specific logic with real transactions

---

## 10. 📘 Glossary of Terms

| Term               | Meaning                                         |
| ------------------ | ----------------------------------------------- |
| **BPS**            | Basis points (1/100 of 1%)                      |
| **Lease**          | Agreement with rent and collateral              |
| **Collateral**     | Locked asset guaranteeing performance           |
| **Assay**          | Verification / evaluation of ore batch contents |
| **NFT**            | Non-fungible token identifying an ore batch     |
| **BuiltinData**    | Raw on-chain Plutus data format                 |
| **ScriptContext**  | Info about the spending transaction             |
| **POSIXTime**      | Timestamp format used in Cardano                |
| **CurrencySymbol** | Identifier for a token policy                   |
| **TokenName**      | Name of a token under a policy                  |
| **txSignedBy**     | Checks if a PubKeyHash signed the transaction   |

---

