import Test.Hspec
import Data.Time (Day, fromGregorian)
import Options.Strategy

day :: Day
day = fromGregorian 2026 9 18

mkLeg :: OptionType -> Side -> Double -> Double -> Leg
mkLeg ot s k prem = Leg { optType = ot, side = s, strike = k, expiry = day, premium = prem }

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

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False
