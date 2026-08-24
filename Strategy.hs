-- | A small typed DSL for options strategies.
--
-- The central idea: instead of representing a strategy as a bare
-- @[Leg]@ (which lets you build nonsense), each strategy shape gets
-- its own type with a smart constructor that enforces the
-- structural invariants (leg roles, strike ordering, matching
-- expiry). Once you hold a value of, say, 'IronCondor', it is
-- guaranteed well-formed — no runtime re-checking needed downstream.
--
-- Every strategy type is an instance of 'Strategy', which just
-- needs to say what its legs are — 'pnlAtExpiry' then comes for
-- free via the typeclass default, and works the same way regardless
-- of which strategy you're holding.
module Options.Strategy
  ( OptionType (..)
  , Side (..)
  , Leg (..)
  , Strategy (..)
    -- * Iron condor
  , IronCondor
  , putLong
  , putShort
  , callShort
  , callLong
  , mkIronCondor
    -- * Vertical spread
  , VerticalSpread
  , vsType
  , longLeg
  , shortLeg
  , mkVerticalSpread
    -- * Strangle
  , Strangle
  , strangleSide
  , putLeg
  , callLeg
  , mkStrangle
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

-- | Anything that can be broken down into a fixed set of legs gets
-- 'pnlAtExpiry' for free. Instances only need to implement 'legs'.
class Strategy a where
  legs :: a -> [Leg]

  pnlAtExpiry :: a -> Double -> Double
  pnlAtExpiry s underlying = sum (map (legPnlAtExpiry underlying) (legs s))

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

sameExpiry :: [Leg] -> Bool
sameExpiry ls = length (nub (map expiry ls)) == 1

-- ---------------------------------------------------------------
-- Iron condor
-- ---------------------------------------------------------------

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

instance Strategy IronCondor where
  legs ic = [putLong ic, putShort ic, callShort ic, callLong ic]

-- | Smart constructor. Returns a descriptive error instead of
-- allowing an ill-formed strategy to be constructed.
mkIronCondor :: Leg -> Leg -> Leg -> Leg -> Either String IronCondor
mkIronCondor pl ps cs cl
  | optType pl /= Put  || side pl /= Long  = Left "putLong must be a long put"
  | optType ps /= Put  || side ps /= Short = Left "putShort must be a short put"
  | optType cs /= Call || side cs /= Short = Left "callShort must be a short call"
  | optType cl /= Call || side cl /= Long  = Left "callLong must be a long call"
  | not (sameExpiry [pl, ps, cs, cl])
      = Left "all four legs must share the same expiry"
  | not (strike pl < strike ps && strike ps < strike cs && strike cs < strike cl)
      = Left "strikes must satisfy putLong < putShort < callShort < callLong"
  | otherwise = Right (IronCondor pl ps cs cl)

-- ---------------------------------------------------------------
-- Vertical spread
-- ---------------------------------------------------------------

-- | A vertical spread: two legs of the *same* option type (both
-- calls or both puts), same expiry, different strikes — one long,
-- one short. Covers bull/bear call spreads and bull/bear put
-- spreads; which of those it is falls out of which strike is
-- higher and whether 'vsType' is 'Call' or 'Put', rather than being
-- a separate type for each.
data VerticalSpread = VerticalSpread
  { vsType   :: OptionType
  , longLeg  :: Leg
  , shortLeg :: Leg
  } deriving (Show, Eq)

instance Strategy VerticalSpread where
  legs vs = [longLeg vs, shortLeg vs]

mkVerticalSpread :: Leg -> Leg -> Either String VerticalSpread
mkVerticalSpread longL shortL
  | side longL /= Long  = Left "first leg must be long"
  | side shortL /= Short = Left "second leg must be short"
  | optType longL /= optType shortL
      = Left "both legs must be the same option type (both calls or both puts)"
  | not (sameExpiry [longL, shortL])
      = Left "both legs must share the same expiry"
  | strike longL == strike shortL
      = Left "legs must have different strikes"
  | otherwise = Right (VerticalSpread (optType longL) longL shortL)

-- ---------------------------------------------------------------
-- Strangle
-- ---------------------------------------------------------------

-- | A strangle: one put, one call, same expiry, same side (both
-- long = long strangle, both short = short strangle), put strike
-- below call strike.
data Strangle = Strangle
  { strangleSide :: Side
  , putLeg       :: Leg
  , callLeg      :: Leg
  } deriving (Show, Eq)

instance Strategy Strangle where
  legs s = [putLeg s, callLeg s]

mkStrangle :: Leg -> Leg -> Either String Strangle
mkStrangle p c
  | optType p /= Put  = Left "first leg must be a put"
  | optType c /= Call = Left "second leg must be a call"
  | side p /= side c
      = Left "both legs must have the same side (both long or both short)"
  | not (sameExpiry [p, c])
      = Left "both legs must share the same expiry"
  | not (strike p < strike c)
      = Left "put strike must be less than call strike"
  | otherwise = Right (Strangle (side p) p c)
