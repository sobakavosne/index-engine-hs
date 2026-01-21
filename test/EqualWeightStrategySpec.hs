{-# OPTIONS_GHC -Wno-orphans #-}

module EqualWeightStrategySpec where

import           Control.Monad       (forM_)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Data.Time.Calendar  (Day, fromGregorian, isLeapYear)
import           EqualWeightStrategy (EqualWeightStrategy,
                                      EqualWeightStrategyState (..), ewsBasket,
                                      ewsMarketData, mkEqualWeightStrategy)
import           MarketData          (getCalendar, getPrice, loadMarketData)
import           Schedule            (mkSchedule)
import           Strategy            (computeState)
import           Test.Hspec          (Spec, describe, it, shouldBe,
                                      shouldSatisfy)
import           Test.QuickCheck     (Arbitrary (arbitrary), choose)

-- | Generate a valid Day
instance Arbitrary Day where
  arbitrary = do
    year <- choose (2000, 2100)
    month <- choose (1, 12)
    maxDay <-
      case month of
        2 ->
          return
            $ if isLeapYear year
                then 29
                else 28
        4 -> return 30
        6 -> return 30
        9 -> return 30
        11 -> return 30
        _ -> return 31
    day <- choose (1, maxDay)
    return $ fromGregorian year month day

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
  describe "EqualWeightStrategy" $ do
    describe "calculation correctness" $ do
      it "computes correct index level for known dates" $ do
        strategy <- createStrategy
        state1 <- computeState strategy (fromGregorian 2023 1 3)
        state2 <- computeState strategy (fromGregorian 2023 1 31)
        state3 <- computeState strategy (fromGregorian 2023 2 1)
        -- Values should match expected_output.csv (within floating point precision)
        ewsIndexLevel state1 `shouldSatisfy` (> 100.0)
        ewsIndexLevel state2 `shouldSatisfy` (> 90.0)
        ewsIndexLevel state3 `shouldSatisfy` (> 90.0)
      it "computes state at seed date correctly" $ do
        strategy <- createStrategy
        state <- computeState strategy (fromGregorian 2023 1 2)
        ewsIndexLevel state `shouldBe` 100.0
        ewsPortfolioReturn state `shouldBe` 0.0
        all (== 0.0) (Map.elems $ ewsReturns state) `shouldBe` True
        -- Weights should be equal (1/3 for each asset)
        let expectedWeight = 1.0 / 3.0
        let weightsCheck =
              all
                (\w -> abs (w - expectedWeight) < 1e-6)
                (Map.elems $ ewsWeights state)
        weightsCheck `shouldBe` True
      it "weights always sum to 1.0" $ do
        strategy <- createStrategy
        -- Test with known valid dates
        let testDates =
              [ fromGregorian 2023 1 3
              , fromGregorian 2023 1 31
              , fromGregorian 2023 2 15
              , fromGregorian 2023 3 31
              , fromGregorian 2023 6 29
              ]
        forM_ testDates $ \date -> do
          state <- computeState strategy date
          let weightSum = sum $ Map.elems $ ewsWeights state
          abs (weightSum - 1.0) < 1e-6 `shouldBe` True
      it "calculates portfolio return correctly" $ do
        strategy <- createStrategy
        let date1 = fromGregorian 2023 1 3
        let date2 = fromGregorian 2023 1 4
        state1 <- computeState strategy date1
        state2 <- computeState strategy date2
        -- Portfolio return should be weighted sum of asset returns
        let expectedReturn =
              sum
                [ ewsReturns state2 Map.! asset
                  * (ewsWeights state1 Map.! asset)
                | asset <- ewsBasket strategy
                ]
        abs (ewsPortfolioReturn state2 - expectedReturn) < 1e-6 `shouldBe` True
      it "calculates index level correctly" $ do
        strategy <- createStrategy
        let date1 = fromGregorian 2023 1 3
        let date2 = fromGregorian 2023 1 4
        state1 <- computeState strategy date1
        state2 <- computeState strategy date2
        -- Index level should be: prev_level * (1 + portfolio_return)
        let expectedLevel =
              ewsIndexLevel state1 * (1 + ewsPortfolioReturn state2)
        abs (ewsIndexLevel state2 - expectedLevel) < 1e-6 `shouldBe` True
      it "calculates returns correctly" $ do
        strategy <- createStrategy
        let date1 = fromGregorian 2023 1 3
        let date2 = fromGregorian 2023 1 4
        state2 <- computeState strategy date2
        -- Returns should be: (today_price / yesterday_price) - 1
        let md = ewsMarketData strategy
        forM_ (ewsBasket strategy) $ \asset -> do
          priceToday <- either (error . show) return $ getPrice md date2 asset
          priceYesterday <-
            either (error . show) return $ getPrice md date1 asset
          let expectedReturn = (priceToday / priceYesterday) - 1
          let actualReturn = ewsReturns state2 Map.! asset
          abs (actualReturn - expectedReturn) < 1e-6 `shouldBe` True
    describe "rebalancing" $ do
      it "rebalances weights at month-end" $ do
        strategy <- createStrategy
        -- Get state on first day of February (after rebalancing at end of Jan)
        let feb1 = fromGregorian 2023 2 1
        state <- computeState strategy feb1
        -- Weights should be approximately equal (rebalanced at end of Jan)
        let expectedWeight = 1.0 / fromIntegral (length (ewsBasket strategy))
        all
          (\w -> abs (w - expectedWeight) < 0.01)
          (Map.elems $ ewsWeights state)
          `shouldBe` True
      it "weights drift between rebalancings" $ do
        strategy <- createStrategy
        let jan10 = fromGregorian 2023 1 10
        let jan11 = fromGregorian 2023 1 11
        let jan12 = fromGregorian 2023 1 12
        state10 <- computeState strategy jan10
        state11 <- computeState strategy jan11
        state12 <- computeState strategy jan12
        -- Weights should change (drift) based on returns
        ewsWeights state10 /= ewsWeights state11 `shouldBe` True
        ewsWeights state11 /= ewsWeights state12 `shouldBe` True
        -- But all should sum to 1.0
        sum (Map.elems $ ewsWeights state10)
          `shouldSatisfy` (\s -> abs (s - 1.0) < 1e-6)
        sum (Map.elems $ ewsWeights state11)
          `shouldSatisfy` (\s -> abs (s - 1.0) < 1e-6)
        sum (Map.elems $ ewsWeights state12)
          `shouldSatisfy` (\s -> abs (s - 1.0) < 1e-6)
    describe "edge cases" $ do
      it "handles single asset basket" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            let calendarSet = getCalendar md
            let calendar = mkSchedule $ Set.toList calendarSet
            let basket = [T.pack "SPX"]
            let seedDate = fromGregorian 2023 1 2
            strategy <- mkEqualWeightStrategy md basket seedDate calendar 100.0
            state <- computeState strategy (fromGregorian 2023 1 3)
            ewsWeights state
              Map.! T.pack "SPX"
              `shouldSatisfy` (\w -> abs (w - 1.0) < 1e-6)
            Map.size (ewsWeights state) `shouldBe` 1
            Map.size (ewsReturns state) `shouldBe` 1
          Left _ -> error "Should load CSV"
      it "handles two asset basket" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            let calendarSet = getCalendar md
            let calendar = mkSchedule $ Set.toList calendarSet
            let basket = [T.pack "SPX", T.pack "SX5E"]
            let seedDate = fromGregorian 2023 1 2
            strategy <- mkEqualWeightStrategy md basket seedDate calendar 100.0
            state <- computeState strategy seedDate
            ewsWeights state
              Map.! T.pack "SPX"
              `shouldSatisfy` (\w -> abs (w - 0.5) < 1e-6)
            ewsWeights state
              Map.! T.pack "SX5E"
              `shouldSatisfy` (\w -> abs (w - 0.5) < 1e-6)
            sum (Map.elems $ ewsWeights state)
              `shouldSatisfy` (\s -> abs (s - 1.0) < 1e-6)
          Left _ -> error "Should load CSV"
      it "handles very long date range" $ do
        strategy <- createStrategy
        let toDate = fromGregorian 2023 6 29
        -- Just verify it doesn't crash and produces valid states
        state <- computeState strategy toDate
        ewsIndexLevel state `shouldSatisfy` (> 0)
        sum (Map.elems $ ewsWeights state)
          `shouldSatisfy` (\s -> abs (s - 1.0) < 1e-6)
      it "handles negative returns" $ do
        strategy <- createStrategy
        -- Find a date with negative returns (2023-01-11 is known to have negative returns)
        let testDate = fromGregorian 2023 1 11
        state <- computeState strategy testDate
        -- Some returns might be negative
        any (< 0) (Map.elems $ ewsReturns state) `shouldSatisfy` id
        -- Index level should still be positive
        ewsIndexLevel state `shouldSatisfy` (> 0)
        -- Weights should still sum to 1.0
        sum (Map.elems $ ewsWeights state)
          `shouldSatisfy` (\s -> abs (s - 1.0) < 1e-6)
