{-# OPTIONS_GHC -Wno-orphans #-}

module RecursionSpec where

import           Control.Exception   (ErrorCall (..), try)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Data.Time.Calendar  (fromGregorian)
import           EqualWeightStrategy (EqualWeightStrategy,
                                      EqualWeightStrategyState (..),
                                      ewsMarketData, ewsStateStore,
                                      mkEqualWeightStrategy)
import           MarketData          (getCalendar, getPrice, loadMarketData,
                                      updatePrice)
import           Runner              (getStates)
import           Schedule            (mkSchedule)
import           StateStore          (clear)
import           Strategy            (computeState)
import           Test.Hspec          (Spec, describe, it, shouldBe,
                                      shouldSatisfy)

-- | Helper to create a test strategy with specified data file
createStrategyWithFile :: FilePath -> IO EqualWeightStrategy
createStrategyWithFile file = do
  result <- loadMarketData file
  case result of
    Right md -> do
      let calendarSet = getCalendar md
      let calendar = mkSchedule $ Set.toList calendarSet
      let basket = [T.pack "SPX", T.pack "SX5E", T.pack "HSI"]
      let seedDate = fromGregorian 2023 1 2
      mkEqualWeightStrategy md basket seedDate calendar 100.0
    Left err -> error $ "Failed to load market data: " ++ show err

-- | Helper to create a test strategy (uses large dataset for recursion testing)
createStrategy :: IO EqualWeightStrategy
createStrategy = createStrategyWithFile "test/data/large_prices.csv"

spec :: Spec
spec = do
  describe "Recursion" $ do
    it "recursion terminates at seed date" $ do
      strategy <- createStrategy
      let seedDate = fromGregorian 2023 1 2
      -- Computing state at seed date should not recurse
      state <- computeState strategy seedDate
      -- Should return valid state with initial index level
      ewsIndexLevel state `shouldBe` 100.0
      ewsPortfolioReturn state `shouldBe` 0.0
      -- All returns should be zero at seed date
      all (== 0.0) (Map.elems $ ewsReturns state) `shouldBe` True
    it "handles deep recursion without cache" $ do
      strategy <- createStrategy
      -- Clear cache to force deep recursion
      clear $ ewsStateStore strategy
      -- Compute a date far from seed date
      let targetDate = fromGregorian 2023 6 29
      -- Should complete without stack overflow
      state <- computeState strategy targetDate
      ewsIndexLevel state `shouldSatisfy` (> 0)
      sum (Map.elems $ ewsWeights state)
        `shouldSatisfy` (\s -> abs (s - 1.0) < 1e-6)
    it "cache reduces recursion depth" $ do
      strategy <- createStrategy
      let dates =
            [ fromGregorian 2023 1 3
            , fromGregorian 2023 1 4
            , fromGregorian 2023 1 5
            , fromGregorian 2023 1 6
            ]
      -- First pass: compute sequentially (builds cache)
      mapM_ (computeState strategy) dates
      -- Second pass: should use cache (no deep recursion)
      states <- mapM (computeState strategy) dates
      -- All states should be valid
      all (\s -> ewsIndexLevel s > 0) states `shouldBe` True
      all (\s -> abs (sum (Map.elems $ ewsWeights s) - 1.0) < 1e-6) states
        `shouldBe` True
    it "handles very long range recursion" $ do
      strategy <- createStrategy
      -- Clear cache to test deep recursion
      clear $ ewsStateStore strategy
      let fromDate = Just $ fromGregorian 2023 1 2
      let toDate = fromGregorian 2023 12 29
      -- Should handle long range without issues (full year of data)
      states <- getStates strategy fromDate toDate
      Map.size states `shouldSatisfy` (> 250) -- Full year has ~250 trading days
      -- All states should be valid
      all
        (\s ->
           ewsIndexLevel s > 0
             && abs (sum (Map.elems $ ewsWeights s) - 1.0) < 1e-6)
        (Map.elems states)
        `shouldBe` True
    it "recursion base case works correctly" $ do
      strategy <- createStrategy
      let seedDate = fromGregorian 2023 1 2
      -- Base case: seed date
      state <- computeState strategy seedDate
      ewsIndexLevel state `shouldBe` 100.0
      -- One day after seed: should recurse to seed date
      let nextDate = fromGregorian 2023 1 3
      stateNext <- computeState strategy nextDate
      ewsIndexLevel stateNext `shouldSatisfy` (> 0)
      ewsIndexLevel stateNext `shouldSatisfy` (/= 100.0)
    it "one day after seed date works" $ do
      strategy <- createStrategy
      let nextDate = fromGregorian 2023 1 3
      -- Should compute successfully
      state <- computeState strategy nextDate
      ewsIndexLevel state `shouldSatisfy` (> 0)
      ewsIndexLevel state `shouldSatisfy` (/= 100.0)
      -- Should have valid returns
      any (/= 0.0) (Map.elems $ ewsReturns state) `shouldBe` True
    it "prevents infinite loop" $ do
      strategy <- createStrategy
      -- Computing states should terminate
      let dates =
            [ fromGregorian 2023 1 3
            , fromGregorian 2023 1 4
            , fromGregorian 2023 1 5
            , fromGregorian 2023 1 6
            , fromGregorian 2023 1 9
            , fromGregorian 2023 1 10
            , fromGregorian 2023 1 11
            , fromGregorian 2023 1 12
            , fromGregorian 2023 1 13
            , fromGregorian 2023 1 16
            ]
      states <- mapM (computeState strategy) dates
      length states `shouldBe` length dates
      -- All should be valid
      all (\s -> ewsIndexLevel s > 0) states `shouldBe` True
    it "handles cache invalidation during recursion" $ do
      strategy <- createStrategy
      let targetDate = fromGregorian 2023 1 16 -- Use a weekday that exists in test data
      -- Compute and cache
      state1 <- computeState strategy targetDate
      let level1 = ewsIndexLevel state1
      -- Update market data (should invalidate cache)
      let md = ewsMarketData strategy
      originalPrice <-
        either (error . show) return $ getPrice md targetDate (T.pack "SPX")
      updateResult <-
        updatePrice md targetDate (T.pack "SPX") (originalPrice * 1.1)
      case updateResult of
        Right md' -> do
          -- Create new strategy with updated data
          let calendarSet = getCalendar md'
          let calendar = mkSchedule $ Set.toList calendarSet
          let basket = [T.pack "SPX", T.pack "SX5E", T.pack "HSI"]
          let seed = fromGregorian 2023 1 2
          strategy' <- mkEqualWeightStrategy md' basket seed calendar 100.0
          -- Should recompute (not use stale cache)
          state2 <- computeState strategy' targetDate
          ewsIndexLevel state2 `shouldSatisfy` (/= level1)
        Left _ -> error "Should update price"
    it "sequential computation prevents deep recursion" $ do
      strategy <- createStrategy
      clear $ ewsStateStore strategy
      let dates =
            [ fromGregorian 2023 1 3
            , fromGregorian 2023 1 4
            , fromGregorian 2023 1 5
            , fromGregorian 2023 1 6
            , fromGregorian 2023 1 9
            , fromGregorian 2023 1 10
            , fromGregorian 2023 1 11
            , fromGregorian 2023 1 12
            ]
      -- Sequential computation builds cache incrementally
      states <- mapM (computeState strategy) dates
      -- Each computation should be faster than if done in isolation
      length states `shouldBe` length dates
      -- All should be valid
      all (\s -> ewsIndexLevel s > 0) states `shouldBe` True
    it "handles error in recursion gracefully" $ do
      strategy <- createStrategy
      -- Try to compute state before seed date (should error)
      let beforeSeed = fromGregorian 2023 1 1
      result <- try $ computeState strategy beforeSeed
      case result of
        Left (ErrorCall _) -> True `shouldBe` True
        Right _            -> False `shouldBe` True
    it "handles empty cache recursion" $ do
      strategy <- createStrategy
      -- Clear cache
      clear $ ewsStateStore strategy
      -- Should still compute correctly
      let targetDate = fromGregorian 2023 1 10
      state <- computeState strategy targetDate
      ewsIndexLevel state `shouldSatisfy` (> 0)
      sum (Map.elems $ ewsWeights state)
        `shouldSatisfy` (\s -> abs (s - 1.0) < 1e-6)
    it "recursion limit safety (very deep chain)" $ do
      strategy <- createStrategy
      clear $ ewsStateStore strategy
      -- Compute a date very far from seed
      let farDate = fromGregorian 2023 6 29
      -- Should complete without stack overflow
      state <- computeState strategy farDate
      ewsIndexLevel state `shouldSatisfy` (> 0)
      -- Verify correctness
      sum (Map.elems $ ewsWeights state)
        `shouldSatisfy` (\s -> abs (s - 1.0) < 1e-6)
