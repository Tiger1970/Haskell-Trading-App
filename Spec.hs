import Test.Hspec
import Test.QuickCheck
import Data.Time (Day, fromGregorian)
import Options.Strategy

day :: Day
day = fromGregorian 2026 9 18

mkLeg :: OptionType -> Side -> Double -> Double -> Leg
mkLeg ot s k prem = Leg { optType = ot, side = s, strike = k, expiry = day, premium = prem }

-- | Generates arbitrary but always-VALID condors, so property tests
-- exercise pnlAtExpiry across many strike/premium combos rather than
-- wasting runs on rejected inputs.
genValidCondor :: Gen IronCondor
genValidCondor = do
  base <- choose (50, 200) :: Gen Double
  gap1 <- choose (1, 20)   -- putLong to putShort
  gap2 <- choose (1, 20)   -- putShort to callShort (the "body")
  gap3 <- choose (1, 20)   -- callShort to callLong
  premPL <- choose (0.1, 5)
  premPS <- choose (0.1, 5)
  premCS <- choose (0.1, 5)
  premCL <- choose (0.1, 5)
  let kPL = base
      kPS = base + gap1
      kCS = kPS + gap2
      kCL = kCS + gap3
      Right condor = mkIronCondor
        (mkLeg Put  Long  kPL premPL)
        (mkLeg Put  Short kPS premPS)
        (mkLeg Call Short kCS premCS)
        (mkLeg Call Long  kCL premCL)
  return condor

-- | Net credit received when opening the condor: short premiums
-- received minus long premiums paid.
netCreditOf :: IronCondor -> Double
netCreditOf ic =
  premium (putShort ic) + premium (callShort ic) - premium (putLong ic) - premium (callLong ic)

-- | Max loss: wing width minus net credit. Both wings are the same
-- width by construction is NOT assumed here — each wing is checked
-- independently against its own width.
maxLossPut :: IronCondor -> Double
maxLossPut ic = (strike (putShort ic) - strike (putLong ic)) - netCreditOf ic

maxLossCall :: IronCondor -> Double
maxLossCall ic = (strike (callLong ic) - strike (callShort ic)) - netCreditOf ic

-- A valid condor: put wing at 90/95, call wing at 105/110
validPutLong, validPutShort, validCallShort, validCallLong :: Leg
validPutLong   = mkLeg Put  Long  90  1.0
validPutShort  = mkLeg Put  Short 95  2.0
validCallShort = mkLeg Call Short 105 2.0
validCallLong  = mkLeg Call Long  110 1.0

main :: IO ()
main = hspec $ do
  describe "mkIronCondor" $ do
    it "accepts a correctly ordered, same-expiry condor" $ do
      mkIronCondor validPutLong validPutShort validCallShort validCallLong
        `shouldSatisfy` isRight

    it "rejects legs with the wrong option type in a role" $ do
      let badCallShort = mkLeg Put Short 105 2.0  -- should be a Call
      mkIronCondor validPutLong validPutShort badCallShort validCallLong
        `shouldBe` Left "callShort must be a short call"

    it "rejects out-of-order strikes" $ do
      let badPutShort = mkLeg Put Short 92 2.0  -- fine on its own, but test crossed strikes below
          badCallShort = mkLeg Call Short 91 2.0 -- crosses putShort
      mkIronCondor validPutLong badPutShort badCallShort validCallLong
        `shouldBe` Left "strikes must satisfy putLong < putShort < callShort < callLong"

    it "rejects mismatched expiries" $ do
      let otherDay = fromGregorian 2026 10 16
          mismatched = validCallLong { expiry = otherDay }
      mkIronCondor validPutLong validPutShort validCallShort mismatched
        `shouldBe` Left "all four legs must share the same expiry"

  describe "pnlAtExpiry" $ do
    let Right condor = mkIronCondor validPutLong validPutShort validCallShort validCallLong
        -- net credit received at entry: (-1.0 + 2.0) + (2.0 - 1.0) = 2.0
        netCredit = 2.0

    it "yields max profit (net credit) when underlying settles between short strikes" $ do
      pnlAtExpiry condor 100 `shouldBe` netCredit

    it "yields max loss (wing width - net credit) when underlying settles below put wing" $ do
      -- below 90: put spread pays out (95-90)=5 max loss, minus credit received
      pnlAtExpiry condor 85 `shouldBe` (netCredit - 5)

    it "yields max loss (wing width - net credit) when underlying settles above call wing" $ do
      -- above 110: call spread pays out (110-105)=5 max loss, minus credit received
      pnlAtExpiry condor 115 `shouldBe` (netCredit - 5)

  describe "pnlAtExpiry properties" $ do
    it "never exceeds net credit (max profit) at any underlying price" $
      property $ forAll genValidCondor $ \ic ->
        forAll (choose (0, 1000)) $ \underlying ->
          pnlAtExpiry ic underlying <= netCreditOf ic + 1e-9  -- float tolerance

    it "is exactly the net credit for any underlying strictly between the short strikes" $
      property $ forAll genValidCondor $ \ic ->
        forAll (choose (strike (putShort ic), strike (callShort ic))) $ \underlying ->
          abs (pnlAtExpiry ic underlying - netCreditOf ic) < 1e-9

    it "never loses more than the worse of the two wings' max loss" $
      property $ forAll genValidCondor $ \ic ->
        forAll (choose (0, 1000)) $ \underlying ->
          pnlAtExpiry ic underlying >= min (-(maxLossPut ic)) (-(maxLossCall ic)) - 1e-9

  describe "mkIronCondor properties" $ do
    it "always rejects strikes that are not strictly increasing" $
      property $ \kpl kps kcs kcl ->
        not (kpl < kps && kps < kcs && kcs < (kcl :: Double)) ==>
          case mkIronCondor (mkLeg Put Long kpl 1) (mkLeg Put Short kps 1)
                            (mkLeg Call Short kcs 1) (mkLeg Call Long kcl 1) of
            Left _  -> True
            Right _ -> False

  describe "mkVerticalSpread" $ do
    it "accepts a valid bull call spread (long lower strike, short higher strike)" $ do
      mkVerticalSpread (mkLeg Call Long 100 3.0) (mkLeg Call Short 110 1.0)
        `shouldSatisfy` isRight

    it "accepts a valid bear put spread (long higher strike, short lower strike)" $ do
      mkVerticalSpread (mkLeg Put Long 100 3.0) (mkLeg Put Short 90 1.0)
        `shouldSatisfy` isRight

    it "rejects mixed option types" $ do
      mkVerticalSpread (mkLeg Call Long 100 3.0) (mkLeg Put Short 110 1.0)
        `shouldBe` Left "both legs must be the same option type (both calls or both puts)"

    it "rejects a long leg that isn't Long" $ do
      mkVerticalSpread (mkLeg Call Short 100 3.0) (mkLeg Call Short 110 1.0)
        `shouldBe` Left "first leg must be long"

    it "rejects identical strikes" $ do
      mkVerticalSpread (mkLeg Call Long 100 3.0) (mkLeg Call Short 100 1.0)
        `shouldBe` Left "legs must have different strikes"

    it "rejects mismatched expiries" $ do
      let otherDay = fromGregorian 2026 10 16
      mkVerticalSpread (mkLeg Call Long 100 3.0) ((mkLeg Call Short 110 1.0) { expiry = otherDay })
        `shouldBe` Left "both legs must share the same expiry"

  describe "vertical spread pnlAtExpiry" $ do
    -- bull call spread: long 100c @3.0, short 110c @1.0 -> debit paid = 2.0
    let Right bullCall = mkVerticalSpread (mkLeg Call Long 100 3.0) (mkLeg Call Short 110 1.0)
        debitPaid = 2.0

    it "yields max loss (debit paid) when underlying settles at or below the long strike" $ do
      pnlAtExpiry bullCall 95 `shouldBe` (-debitPaid)

    it "yields max profit (spread width - debit) when underlying settles at or above the short strike" $ do
      -- width 10, debit 2 -> max profit 8
      pnlAtExpiry bullCall 115 `shouldBe` 8

    it "is between max loss and max profit when underlying settles between the strikes" $ do
      let pnl = pnlAtExpiry bullCall 105
      pnl `shouldSatisfy` (\p -> p > (-debitPaid) && p < 8)

  describe "mkStrangle" $ do
    it "accepts a valid long strangle" $ do
      mkStrangle (mkLeg Put Long 90 1.0) (mkLeg Call Long 110 1.0)
        `shouldSatisfy` isRight

    it "accepts a valid short strangle" $ do
      mkStrangle (mkLeg Put Short 90 1.0) (mkLeg Call Short 110 1.0)
        `shouldSatisfy` isRight

    it "rejects mismatched sides" $ do
      mkStrangle (mkLeg Put Long 90 1.0) (mkLeg Call Short 110 1.0)
        `shouldBe` Left "both legs must have the same side (both long or both short)"

    it "rejects put strike not below call strike" $ do
      mkStrangle (mkLeg Put Long 110 1.0) (mkLeg Call Long 90 1.0)
        `shouldBe` Left "put strike must be less than call strike"

    it "rejects wrong option type in a role" $ do
      mkStrangle (mkLeg Call Long 90 1.0) (mkLeg Call Long 110 1.0)
        `shouldBe` Left "first leg must be a put"

  describe "strangle pnlAtExpiry" $ do
    -- long strangle: long 90p @1.0, long 110c @1.0 -> total premium paid = 2.0
    let Right longStrangle = mkStrangle (mkLeg Put Long 90 1.0) (mkLeg Call Long 110 1.0)
        premiumPaid = 2.0

    it "loses exactly the premium paid when underlying settles between the strikes" $ do
      pnlAtExpiry longStrangle 100 `shouldBe` (-premiumPaid)

    it "profits below the put strike" $ do
      -- underlying 80: put intrinsic = 10, minus premium paid = 8
      pnlAtExpiry longStrangle 80 `shouldBe` 8

    it "profits above the call strike" $ do
      -- underlying 120: call intrinsic = 10, minus premium paid = 8
      pnlAtExpiry longStrangle 120 `shouldBe` 8

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False
