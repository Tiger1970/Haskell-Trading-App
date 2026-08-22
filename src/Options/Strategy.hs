-- | A small typed DSL for options strategies.
--
-- The central idea: instead of representing a strategy as a bare
-- @[Leg]@ (which lets you build nonsense), each strategy shape gets
-- its own type with a smart constructor that enforces the
-- structural invariants (leg roles, strike ordering, matching
-- expiry). Once you hold a value of type 'IronCondor', it is
-- guaranteed well-formed — no runtime re-checking needed downstream.
module Options.Strategy
  ( OptionType (..)
  , Side (..)
  , Leg (..)
  , IronCondor
  , putLong
  , putShort
  , callShort
  , callLong
  , mkIronCondor
  , pnlAtExpiry
  ) where

import Data.Time (Day)
import Data.List (nub)

data OptionType = Call | Put
  deriving (Show, Eq)

data Side = Long | Short
  deriving (Show, Eq)

data Leg = Leg
  { optType :: OptionType
  , side    :: Side
  , strike  :: Double
  , expiry  :: Day
  , premium :: Double
    -- ^ price paid (Long) or received (Short) to enter this leg,
    -- always quoted as a positive number.
  } deriving (Show, Eq)

-- | An iron condor: long put (protection), short put, short call,
-- long call (protection), ordered by strike.
--
-- Constructor is deliberately not exported — the only way to build
-- one is 'mkIronCondor', which enforces the invariants below.
data IronCondor = IronCondor
  { putLong   :: Leg
  , putShort  :: Leg
  , callShort :: Leg
  , callLong  :: Leg
  } deriving (Show, Eq)

-- | Smart constructor. Returns a descriptive error instead of
-- allowing an ill-formed strategy to be constructed.
mkIronCondor :: Leg -> Leg -> Leg -> Leg -> Either String IronCondor
mkIronCondor pl ps cs cl
  | optType pl /= Put  || side pl /= Long  = Left "putLong must be a long put"
  | optType ps /= Put  || side ps /= Short = Left "putShort must be a short put"
  | optType cs /= Call || side cs /= Short = Left "callShort must be a short call"
  | optType cl /= Call || side cl /= Long  = Left "callLong must be a long call"
  | length (nub [expiry pl, expiry ps, expiry cs, expiry cl]) /= 1
      = Left "all four legs must share the same expiry"
  | not (strike pl < strike ps && strike ps < strike cs && strike cs < strike cl)
      = Left "strikes must satisfy putLong < putShort < callShort < callLong"
  | otherwise = Right (IronCondor pl ps cs cl)

-- | Payoff of a single leg at expiry, given the underlying price.
-- Positive = profit, negative = loss. Includes the premium paid or
-- received to enter the leg.
legPnlAtExpiry :: Double -> Leg -> Double
legPnlAtExpiry underlying leg =
  let intrinsic = case optType leg of
        Call -> max 0 (underlying - strike leg)
        Put  -> max 0 (strike leg - underlying)
      directionSign = case side leg of
        Long  -> 1
        Short -> -1
      entryCashflow = case side leg of
        Long  -> -premium leg   -- paid premium
        Short ->  premium leg   -- received premium
  in directionSign * intrinsic + entryCashflow

-- | Total P&L of the condor at expiry for a given underlying price.
pnlAtExpiry :: IronCondor -> Double -> Double
pnlAtExpiry ic underlying =
  sum (map (legPnlAtExpiry underlying) [putLong ic, putShort ic, callShort ic, callLong ic])
