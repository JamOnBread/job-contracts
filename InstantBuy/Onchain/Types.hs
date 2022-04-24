{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module InstantBuy.Onchain.Types
  ( Listing (..),
    Operation (..),
    Royalty (..),
    Price,
  )
where

import Cardano.Api.Byron (ToJSON)
import Data.Aeson (FromJSON)
import Data.Maybe
import GHC.Generics (Generic)
import Ledger (Address, AssetClass, PubKeyHash)
import qualified PlutusTx
import PlutusTx.Prelude as Plutus (Integer)
import Prelude (Eq (..), Show (..), (&&))

type Price = Integer

-- We use basis to account for 3 decimal places
-- When the royalty fee is 5.6%, then rBasis = 560
data Royalty = Royalty
  { rBasis :: !Integer,
    rAddr :: !Address
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Eq Royalty where
  {-# INLINEABLE (==) #-}
  a == b =
    rBasis a == rBasis b
      && rAddr a == rAddr b

PlutusTx.makeIsDataIndexed ''Royalty [('Royalty, 0)]
PlutusTx.makeLift ''Royalty

-- nftPrice is the full price of the sale. From that, royalty
-- and treasury fee are deducted.
-- The seller only receives (nftPrice - royalty - treasuryFee)
data Listing = Listing
  { sellerAddr :: !Address,
    nftPrice :: !Price,
    nftAsset :: !AssetClass,
    royalty :: !(Maybe Royalty)
  }
  deriving (Show)

PlutusTx.makeIsDataIndexed ''Listing [('Listing, 0)]
PlutusTx.makeLift ''Listing

data Operation = Buy | Cancel deriving (Show)

PlutusTx.makeIsDataIndexed ''Operation [('Buy, 0), ('Cancel, 1)]
PlutusTx.makeLift ''Operation
