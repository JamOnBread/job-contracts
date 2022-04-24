{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module InstantBuy.Onchain.Utils
  ( treasuryFee,
    adaAsset,
    getRoyaltyLovelace,
    isScriptOutput,
    outputContainsAnyDatum,
  )
where

import InstantBuy.Onchain.ProtocolParams as P
import InstantBuy.Onchain.Types (Price, Royalty (..))
import Ledger
import Ledger.Value as Value
  ( AssetClass (unAssetClass),
    assetClass,
  )
import qualified Plutus.V1.Ledger.Ada as Ada
import Plutus.V1.Ledger.Contexts as Contexts (valuePaidTo)
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Crypto (PubKeyHash (PubKeyHash))
import PlutusTx.Prelude

-- If treasury fee is too low, we ignore it
{-# INLINEABLE treasuryFee #-}
treasuryFee :: Price -> Integer
treasuryFee price =
  if fee < P.ignoreTreasuryFeeUntil then 0 else fee
  where
    fee = (price * P.feeBasis) `divide` 10_000

{-# INLINEABLE adaAsset #-}
adaAsset :: AssetClass
adaAsset = assetClass Ada.adaSymbol Ada.adaToken

-- If the royalty value is too low, artist does not get anything (min UTxO value problems ...)
-- In case royaltyAddress == sellerAddress, we ignore royalties
{-# INLINEABLE getRoyaltyLovelace #-}
getRoyaltyLovelace :: Maybe Royalty -> Price -> Address -> Integer
getRoyaltyLovelace maybeRoyalty price sellerAddr =
  case maybeRoyalty of
    Nothing -> 0
    Just royalty ->
      if rAddr royalty == sellerAddr || lovelace < P.ignoreRoyaltyFeeUntil
        then 0
        else lovelace
      where
        lovelace = (price * rBasis royalty) `divide` 10_000

{-# INLINEABLE isScriptOutput #-}
isScriptOutput :: TxOut -> Bool
isScriptOutput out =
  case addressCredential (txOutAddress out) of
    (ScriptCredential _) -> True
    _ -> False

{-# INLINEABLE outputContainsAnyDatum #-}
outputContainsAnyDatum :: TxOut -> Bool
outputContainsAnyDatum o = isJust $ txOutDatumHash o
