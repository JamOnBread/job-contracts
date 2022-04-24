{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module InstantBuy.Onchain.InstantBuy (mkMarketValidator) where

import InstantBuy.Onchain.Types
  ( Listing (..),
    Operation (..),
    Royalty (..),
  )
import InstantBuy.Onchain.Utils
  ( adaAsset,
    getRoyaltyLovelace,
    isScriptOutput,
    outputContainsAnyDatum,
    treasuryFee,
  )
import Ledger
import qualified Plutus.V1.Ledger.Ada as Ada
import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Contexts as Contexts (txOutDatumHash)
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Value (assetClassValueOf)
import qualified PlutusTx
import PlutusTx.Prelude as Plutus

-- GENERAL CHECKS
-- 1) Only allow 1 script input to avoid double satisfaction problem
-- 2) Forbid datum in pubkey outputs, although node ignores such datums for now
-- BUY
-- 3) Enough ADA is paid to the seller
-- 4) Price is non-negative
-- 5) Treasury receives fee
-- 6) Royalty is paid
-- CANCEL
-- 7) Seller signed the transaction
-- Parametrize validator by the treasuryAddr. The fee will be sent there.
{-# INLINEABLE mkMarketValidator #-}
mkMarketValidator :: Address -> Listing -> Operation -> ScriptContext -> Bool
mkMarketValidator treasuryAddr Listing {..} operation ctx =
  {--} traceIfFalse "More than one script input in Tx" onlyOneScriptInput
    && traceIfFalse "A PubKey output with datum exists" noPubKeyOutputWithDatum
    && case operation of
      Buy ->
        {--} traceIfFalse "Not enough ADA to buy this listing" enoughAdaToSeller
          && traceIfFalse "Price is negative" (nftPrice >= 0)
          && traceIfFalse "Treasury fee not high enough" treasuryReceivesFee
          && traceIfFalse "Royalty not paid" royaltyPaid
      Cancel -> traceIfFalse "Only seller can cancel the listing" sellerSigned
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    onlyOneScriptInput :: Bool
    onlyOneScriptInput = length [i | i <- txInfoInputs info, isScriptOutput (txInInfoResolved i)] == 1

    royaltyLovelace :: Integer
    royaltyLovelace = getRoyaltyLovelace royalty nftPrice sellerAddr

    treasuryLovelace :: Integer
    treasuryLovelace = treasuryFee nftPrice

    outToAddr :: Address -> TxOut
    outToAddr addr =
      case [o | o <- txInfoOutputs info, txOutAddress o == addr] of
        [o] -> o
        _ -> traceError "Expected exactly 1 output for 1 address"

    noPubKeyOutputWithDatum :: Bool
    noPubKeyOutputWithDatum = length [o | o <- txInfoOutputs info, not (isScriptOutput o) && outputContainsAnyDatum o] == 0

    -- We find the input holding the correct NFT and return all ADA in that NFT (deposit) to the seller
    -- together with the listing price minus treasury and royalty fees
    enoughAdaToSeller :: Bool
    enoughAdaToSeller =
      case ins of
        [nftInp] ->
          assetClassValueOf (txOutValue sellerOut) adaAsset
            >= nftPrice + assetClassValueOf (txOutValue nftInp) adaAsset - treasuryLovelace - royaltyLovelace
        _ -> False
      where
        ins = [i | i <- map txInInfoResolved (txInfoInputs info), assetClassValueOf (txOutValue i) nftAsset == 1]
        sellerOut = outToAddr sellerAddr

    -- If seller signed the transaction, it means they agree with how their NFT and ADA are spent.
    -- Therefore, there is no need to restrict cancel in any way other then checking for seller's signature
    sellerSigned :: Bool
    sellerSigned =
      case toPubKeyHash sellerAddr of
        (Just sellerPkh) -> txSignedBy info sellerPkh
        Nothing -> traceError "Seller is a script address"

    treasuryReceivesFee :: Bool
    treasuryReceivesFee =
      if treasuryLovelace == 0
        then True
        else assetClassValueOf (txOutValue treasuryOut) adaAsset >= treasuryLovelace
      where
        treasuryOut = outToAddr treasuryAddr

    royaltyPaid :: Bool
    royaltyPaid =
      case royalty of
        Nothing -> True
        Just r ->
          if royaltyLovelace == 0
            then True
            else assetClassValueOf (txOutValue royaltyOut) adaAsset >= royaltyLovelace
          where
            royaltyOut = outToAddr $ rAddr r
