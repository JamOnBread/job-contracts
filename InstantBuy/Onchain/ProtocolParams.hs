{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module InstantBuy.Onchain.ProtocolParams
  ( feeBasis,
    ignoreTreasuryFeeUntil,
    ignoreRoyaltyFeeUntil,
  )
where

import PlutusTx.Prelude

--  2% of each sale goes into the treasury
-- Basis so we account for 2 decimal places
{-# INLINEABLE feeBasis #-}
feeBasis :: Integer
feeBasis = 200

-- When the treasury fee is less than this many Lovelace, we ignore it
{-# INLINEABLE ignoreTreasuryFeeUntil #-}
ignoreTreasuryFeeUntil :: Integer
ignoreTreasuryFeeUntil = 1_000_000

-- When the royalty fee is less than this many Lovelace, we ignore it
{-# INLINEABLE ignoreRoyaltyFeeUntil #-}
ignoreRoyaltyFeeUntil :: Integer
ignoreRoyaltyFeeUntil = 1_000_000
