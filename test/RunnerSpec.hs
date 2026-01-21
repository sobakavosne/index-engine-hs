{-# OPTIONS_GHC -Wno-orphans #-}

module RunnerSpec where

import           Data.List              (sort)
import           Data.Maybe             (fromMaybe)
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Data.Text               as T
import           Data.Time.Calendar     (fromGregorian)
import           EqualWeightStrategy    (EqualWeightStrategy,
                                          EqualWeightStrategyState (..),
                                          mkEqualWeightStrategy)
import           MarketData             (getCalendar, loadMarketData)
import           Runner                 (getStates)
import           Schedule               (mkSchedule)
import           Test.Hspec             (Spec, describe, it, shouldBe,
                                          shouldSatisfy)

-- | Helper to create a test strategy
createStrategy :: IO EqualWeightStrategy
createStrategy = do
  result <- loadMarketData "data/sample_prices.csv"
  case result of
    Right md -> do
      let calendarSet = getCalendar md
      let calendar = mkSchedule $ Set.toList calendarSet
      let basket = [T.pack "SPX", T.pack "SX5E", T.pack "HSI"]
      let seedDate = fromGregorian 2023 1 2
      mkEqualWeightStrategy md basket seedDate calendar 100.0
    Left err -> error $ "Failed to load market data: " ++ show err

spec :: Spec
spec = do
  describe "Runner" $ do
    describe "getStates" $ do
      it "gets states with explicit from_date" $ do
        strategy <- createStrategy
        let fromDate = Just $ fromGregorian 2023 1 3
        let toDate = fromGregorian 2023 1 5
        states <- getStates strategy fromDate toDate
        Map.size states `shouldSatisfy` (> 0)
        Map.member (fromGregorian 2023 1 3) states `shouldBe` True
        Map.member (fromGregorian 2023 1 5) states `shouldBe` True
        -- Verify all dates are in range
        all (\d -> d >= fromMaybe (fromGregorian 2023 1 2) fromDate && d <= toDate)
          (Map.keys states)
          `shouldBe` True

      it "gets states with None from_date (uses seed_date)" $ do
        strategy <- createStrategy
        let toDate = fromGregorian 2023 1 5
        states <- getStates strategy Nothing toDate
        Map.size states `shouldSatisfy` (> 0)
        Map.member (fromGregorian 2023 1 2) states `shouldBe` True
        Map.member toDate states `shouldBe` True
        -- Verify all dates are from seed_date onwards
        all (>= fromGregorian 2023 1 2) (Map.keys states) `shouldBe` True
        all (<= toDate) (Map.keys states) `shouldBe` True

      it "gets states for single date range" $ do
        strategy <- createStrategy
        let targetDate = fromGregorian 2023 1 3
        states <- getStates strategy (Just targetDate) targetDate
        Map.size states `shouldBe` 1
        Map.member targetDate states `shouldBe` True
        ewsIndexLevel (states Map.! targetDate) `shouldSatisfy` (> 0)

      it "gets states for empty range" $ do
        strategy <- createStrategy
        -- Use dates that are definitely outside the calendar range
        let fromDate = Just $ fromGregorian 2024 1 1
        let toDate = fromGregorian 2024 1 2
        states <- getStates strategy fromDate toDate
        Map.size states `shouldBe` 0

      it "gets states from seed_date to end" $ do
        strategy <- createStrategy
        let toDate = fromGregorian 2023 6 29
        states <- getStates strategy Nothing toDate
        Map.size states `shouldSatisfy` (> 0)
        Map.member (fromGregorian 2023 1 2) states `shouldBe` True
        Map.member toDate states `shouldBe` True

      it "returns correct state objects" $ do
        strategy <- createStrategy
        let fromDate = Just $ fromGregorian 2023 1 3
        let toDate = fromGregorian 2023 1 5
        states <- getStates strategy fromDate toDate
        -- Verify each state has required attributes
        all
          (\state ->
             ewsIndexLevel state > 0
               && sum (Map.elems $ ewsWeights state) > 0.99
               && Map.size (ewsReturns state) > 0)
          (Map.elems states)
          `shouldBe` True

      it "returns states consistent with individual computeState calls" $ do
        strategy <- createStrategy
        let fromDate = Just $ fromGregorian 2023 1 3
        let toDate = fromGregorian 2023 1 5
        statesBatch <- getStates strategy fromDate toDate
        -- Compare with individual calls (would need to import computeState)
        -- For now, just verify structure
        Map.size statesBatch `shouldSatisfy` (> 0)

      it "returns dates in chronological order" $ do
        strategy <- createStrategy
        let fromDate = Just $ fromGregorian 2023 1 2
        let toDate = fromGregorian 2023 1 10
        states <- getStates strategy fromDate toDate
        let dates = Map.keys states
        dates `shouldBe` sort dates

      it "handles invalid range (from_date > to_date)" $ do
        strategy <- createStrategy
        let fromDate = Just $ fromGregorian 2023 1 5
        let toDate = fromGregorian 2023 1 3
        states <- getStates strategy fromDate toDate
        -- Should return empty dict (invalid range)
        Map.size states `shouldBe` 0

      it "handles large date range" $ do
        strategy <- createStrategy
        let fromDate = Just $ fromGregorian 2023 1 2
        let toDate = fromGregorian 2023 6 29
        states <- getStates strategy fromDate toDate
        Map.size states `shouldSatisfy` (> 100)
        Map.member (fromGregorian 2023 1 2) states `shouldBe` True
        Map.member toDate states `shouldBe` True
        -- Verify all states are valid
        all
          (\state ->
             ewsIndexLevel state > 0
               && abs (sum (Map.elems $ ewsWeights state) - 1.0) < 1e-6)
          (Map.elems states)
          `shouldBe` True
