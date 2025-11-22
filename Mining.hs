{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Main where

import Prelude (IO, print, putStrLn, String)
import qualified Prelude as H

import PlutusTx
import PlutusTx.Prelude        hiding (Semigroup(..), unless, ($))
import Plutus.V2.Ledger.Api
  ( BuiltinData
  , ScriptContext (..)
  , TxInfo (..)
  , TxOut (..)
  , Validator
  , mkValidatorScript
  , PubKeyHash
  , Address (..)
  , Credential (..)
  , POSIXTime
  , CurrencySymbol
  , TokenName
  , txInfoValidRange
  )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Interval (contains, from)
import qualified Plutus.V1.Ledger.Value as Value

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)

import Cardano.Api (writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)

--------------------------------------------------------------------------------
-- Datum & Redeemer
--------------------------------------------------------------------------------

-- Royalty entry: beneficiary pubkey + basis points (0..10000)
data RoyaltyEntry = RoyaltyEntry
    { reBenef   :: PubKeyHash
    , reBps     :: Integer
    }
PlutusTx.unstableMakeIsData ''RoyaltyEntry

-- RoyaltySplitter: operator (who submits settlements) and list of entries, payout token
data RoyaltySplitter = RoyaltySplitter
    { rsOperator  :: PubKeyHash
    , rsEntries   :: [RoyaltyEntry]
    , rsPayoutCS  :: CurrencySymbol
    , rsPayoutTN  :: TokenName
    }
PlutusTx.unstableMakeIsData ''RoyaltySplitter

-- Lease datum: lessor, lessee, rent amount (per period), collateral amount (token units),
-- collateral token id, lease end time (POSIXTime)
data LeaseDatum = LeaseDatum
    { ldLessor        :: PubKeyHash
    , ldLessee        :: PubKeyHash
    , ldRentAmount    :: Integer
    , ldCollateralAmt :: Integer
    , ldCollateralCS  :: CurrencySymbol
    , ldCollateralTN  :: TokenName
    , ldLeaseEnd      :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''LeaseDatum

-- OreBatch NFT datum: owner, source identifier, latest assay/provenance hash
data OreBatchDatum = OreBatchDatum
    { obOwner     :: PubKeyHash
    , obSource    :: BuiltinByteString
    , obAssayHash :: BuiltinByteString
    , obNftCS     :: CurrencySymbol
    , obNftTN     :: TokenName
    }
PlutusTx.unstableMakeIsData ''OreBatchDatum

-- Redeemers / actions
data MiningAction =
      DistributeRoyalties Integer POSIXTime     -- productionAmount, settlementTime
    | PayRent Integer POSIXTime                 -- amountPaid, paidAt
    | SeizeCollateral                           -- lessor seizes collateral (after leaseEnd)
    | EndLease POSIXTime                         -- end lease after leaseEnd (signed)
    | TransferBatch PubKeyHash                  -- new owner for the NFT
    | AppendAssay BuiltinByteString             -- append/replace assay hash (owner signed)
PlutusTx.unstableMakeIsData ''MiningAction

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE pubKeyHashAddress #-}
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = Address (PubKeyCredential pkh) Nothing

{-# INLINABLE valuePaidTo #-}
-- Sum amount of (cs,tn) paid to a pubkey in tx outputs
valuePaidTo :: TxInfo -> PubKeyHash -> CurrencySymbol -> TokenName -> Integer
valuePaidTo info pkh cs tn =
    let outs = txInfoOutputs info
        matches = [ Value.valueOf (txOutValue o) cs tn
                  | o <- outs
                  , txOutAddress o == pubKeyHashAddress pkh
                  ]
    in foldr (+) 0 matches

{-# INLINABLE nowInRange #-}
-- Check if tx's valid range can include a time >= t
nowInRange :: TxInfo -> POSIXTime -> Bool
nowInRange info t = contains (from t) (txInfoValidRange info)

{-# INLINABLE sumBps #-}
sumBps :: [RoyaltyEntry] -> Integer
sumBps rs = foldr (\e acc -> reBps e + acc) 0 rs

{-# INLINABLE shareOf #-}
-- Compute share = (production * bps) / 10000
shareOf :: Integer -> Integer -> Integer
shareOf production bps = (production * bps) `divide` 10000

{-# INLINABLE allPaid #-}
allPaid :: TxInfo -> [RoyaltyEntry] -> CurrencySymbol -> TokenName -> Integer -> Bool
allPaid info entries cs tn production =
    let checks = [ valuePaidTo info (reBenef e) cs tn >= shareOf production (reBps e) | e <- entries ]
    in foldr (&&) True checks

{-# INLINABLE nftPresentInOutputs #-}
nftPresentInOutputs :: TxInfo -> CurrencySymbol -> TokenName -> Bool
nftPresentInOutputs info cs tn =
    let outs = txInfoOutputs info
        matches = [ Value.valueOf (txOutValue o) cs tn | o <- outs ]
    in foldr (+) 0 matches > 0

--------------------------------------------------------------------------------
-- Core validator
--------------------------------------------------------------------------------

{-# INLINABLE mkMiningValidator #-}
mkMiningValidator :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkMiningValidator d r ctx =
    let info = scriptContextTxInfo ctx
        -- Try to decode each known datum type; if decoding fails, that path returns False.
        tryRoyalty = case fromBuiltinData d :: Maybe RoyaltySplitter of
          Nothing -> False
          Just rs -> case fromBuiltinData r :: Maybe MiningAction of
            Just (DistributeRoyalties production settlementTime) ->
                traceIfFalse "royalty: operator signature required" (txSignedBy info (rsOperator rs))
                && traceIfFalse "royalty: settlement time reachable" (nowInRange info settlementTime)
                && traceIfFalse "royalty: bps sum <= 10000" (sumBps (rsEntries rs) <= 10000)
                && traceIfFalse "royalty: beneficiaries paid" (allPaid info (rsEntries rs) (rsPayoutCS rs) (rsPayoutTN rs) production)
            _ -> False

        tryLease = case fromBuiltinData d :: Maybe LeaseDatum of
          Nothing -> False
          Just ld -> case fromBuiltinData r :: Maybe MiningAction of
            Just (PayRent amt paidAt) ->
                traceIfFalse "rent: lessee signature required" (txSignedBy info (ldLessee ld))
                && traceIfFalse "rent: timestamp reachable" (nowInRange info paidAt)
                && traceIfFalse "rent: amount >= rent" (amt >= ldRentAmount ld)
                && traceIfFalse "rent: lessor receives rent" (valuePaidTo info (ldLessor ld) (ldCollateralCS ld) (ldCollateralTN ld) >= ldRentAmount ld)
            Just SeizeCollateral ->
                traceIfFalse "seize: lessor signature required" (txSignedBy info (ldLessor ld))
                && traceIfFalse "seize: lease ended" (nowInRange info (ldLeaseEnd ld))
                && traceIfFalse "seize: collateral paid to lessor" (valuePaidTo info (ldLessor ld) (ldCollateralCS ld) (ldCollateralTN ld) >= ldCollateralAmt ld)
            Just (EndLease at) ->
                traceIfFalse "endlease: lessor or lessee required" (txSignedBy info (ldLessor ld) || txSignedBy info (ldLessee ld))
                && traceIfFalse "endlease: lease ended" (nowInRange info at)
            _ -> False

        tryOre = case fromBuiltinData d :: Maybe OreBatchDatum of
          Nothing -> False
          Just ob -> case fromBuiltinData r :: Maybe MiningAction of
            Just (TransferBatch newOwner) ->
                traceIfFalse "transfer: owner signature required" (txSignedBy info (obOwner ob))
                && traceIfFalse "transfer: nft present in outputs" (nftPresentInOutputs info (obNftCS ob) (obNftTN ob))
            Just (AppendAssay newHash) ->
                traceIfFalse "append: owner signature required" (txSignedBy info (obOwner ob))
                && traceIfFalse "append: new hash non-empty" (lengthOfByteString newHash > 0)
            _ -> False

    in tryRoyalty || tryLease || tryOre

--------------------------------------------------------------------------------
-- Wrap & compile
--------------------------------------------------------------------------------

{-# INLINABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped d r c =
    let ctx = unsafeFromBuiltinData c :: ScriptContext
    in if mkMiningValidator d r ctx
         then ()
         else traceError "Mining: validation failed"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrapped ||])

--------------------------------------------------------------------------------
-- Write validator to file
--------------------------------------------------------------------------------

saveValidator :: IO ()
saveValidator = do
    let scriptSerialised = serialise validator
        scriptShortBs    = SBS.toShort (LBS.toStrict scriptSerialised)
        plutusScript     = PlutusScriptSerialised scriptShortBs :: PlutusScript PlutusScriptV2
    r <- writeFileTextEnvelope "mining-validator.plutus" Nothing plutusScript
    case r of
      Left err -> print err
      Right () -> putStrLn "Mining validator written to: mining-validator.plutus"

main :: IO ()
main = saveValidator
