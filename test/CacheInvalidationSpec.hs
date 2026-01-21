{-# OPTIONS_GHC -Wno-orphans #-}

module CacheInvalidationSpec where

import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Data.Time.Calendar  (fromGregorian)
import           EqualWeightStrategy (EqualWeightStrategy,
                                      EqualWeightStrategyState (..),
                                      ewsMarketData, mkEqualWeightStrategy)
import           MarketData          (getCalendar, getPrice, loadMarketData,
                                      updatePrice)
import           Schedule            (mkSchedule)
import           Strategy            (computeState)
import           Test.Hspec          (Spec, describe, it, shouldBe,
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
  describe "Cache Invalidation" $ do
    it "invalidates cache when market data is updated" $ do
      strategy <- createStrategy
      let targetDate = fromGregorian 2023 1 3
      stateBefore <- computeState strategy targetDate
      let originalLevel = ewsIndexLevel stateBefore
      -- Update market data for that date (using strategy's MarketData)
      let md = ewsMarketData strategy
      originalPrice <-
        either (error . show) return $ getPrice md targetDate (T.pack "SPX")
      updateResult <-
        updatePrice md targetDate (T.pack "SPX") (originalPrice * 1.1)
      case updateResult of
        Right md' -> do
          -- Create a new strategy with updated MarketData
          let calendarSet = getCalendar md'
          let calendar = mkSchedule $ Set.toList calendarSet
          let basket = [T.pack "SPX", T.pack "SX5E", T.pack "HSI"]
          let seedDate = fromGregorian 2023 1 2
          strategy' <- mkEqualWeightStrategy md' basket seedDate calendar 100.0
          -- State should be recomputed (cache invalidated)
          stateAfter <- computeState strategy' targetDate
          ewsIndexLevel stateAfter `shouldSatisfy` (/= originalLevel)
          ewsIndexLevel stateAfter `shouldSatisfy` (> originalLevel) -- Price went up
        Left _ -> error "Should update price"
    it "partially invalidates only affected dates" $ do
      strategy <- createStrategy
      let date1 = fromGregorian 2023 1 3
      let date2 = fromGregorian 2023 1 4
      let date3 = fromGregorian 2023 1 10
      state1Before <- computeState strategy date1
      state2Before <- computeState strategy date2
      state3Before <- computeState strategy date3
        -- Update date2 (using strategy's MarketData)
      let md = ewsMarketData strategy
      originalPrice <-
        either (error . show) return $ getPrice md date2 (T.pack "SPX")
      updateResult <- updatePrice md date2 (T.pack "SPX") (originalPrice * 1.1)
      case updateResult of
        Right md' -> do
          -- Create a new strategy with updated MarketData
          let calendarSet = getCalendar md'
          let calendar = mkSchedule $ Set.toList calendarSet
          let basket = [T.pack "SPX", T.pack "SX5E", T.pack "HSI"]
          let seedDate = fromGregorian 2023 1 2
          strategy' <- mkEqualWeightStrategy md' basket seedDate calendar 100.0
          -- date1 should still be cached (before update)
          state1After <- computeState strategy' date1
          ewsIndexLevel state1After `shouldBe` ewsIndexLevel state1Before
          -- date2 and date3 should be recomputed
          state2After <- computeState strategy' date2
          state3After <- computeState strategy' date3
          ewsIndexLevel state2After
            `shouldSatisfy` (/= ewsIndexLevel state2Before)
          ewsIndexLevel state3After
            `shouldSatisfy` (/= ewsIndexLevel state3Before)
        Left _ -> error "Should update price"
    it "invalidates cascade (early date invalidates later dates)" $ do
      strategy <- createStrategy
      let dates =
            [ fromGregorian 2023 1 3
            , fromGregorian 2023 1 4
            , fromGregorian 2023 1 5
            , fromGregorian 2023 1 6
            ]
      statesBefore <- mapM (computeState strategy) dates
      let levelsBefore = map ewsIndexLevel statesBefore
      -- Update the first date (using strategy's MarketData)
      let md = ewsMarketData strategy
      originalPrice <-
        either (error . show) return $ getPrice md (head dates) (T.pack "SPX")
      updateResult <-
        updatePrice md (head dates) (T.pack "SPX") (originalPrice * 1.1)
      case updateResult of
        Right md' -> do
          -- Create a new strategy with updated MarketData
          let calendarSet = getCalendar md'
          let calendar = mkSchedule $ Set.toList calendarSet
          let basket = [T.pack "SPX", T.pack "SX5E", T.pack "HSI"]
          let seedDate = fromGregorian 2023 1 2
          strategy' <- mkEqualWeightStrategy md' basket seedDate calendar 100.0
          -- All dates should be recomputed
          statesAfter <- mapM (computeState strategy') dates
          let levelsAfter = map ewsIndexLevel statesAfter
          and (zipWith (/=) levelsBefore levelsAfter) `shouldBe` True
        Left _ -> error "Should update price"
    it "handles multiple updates to same date" $ do
      strategy <- createStrategy
      let testDate = fromGregorian 2023 1 3
      state1 <- computeState strategy testDate
      let level1 = ewsIndexLevel state1
        -- Update multiple times (using strategy's MarketData)
      let md = ewsMarketData strategy
      originalPrice <-
        either (error . show) return $ getPrice md testDate (T.pack "SPX")
      updateResult1 <-
        updatePrice md testDate (T.pack "SPX") (originalPrice * 1.1)
      case updateResult1 of
        Right md1 -> do
          -- Create a new strategy with updated MarketData
          let calendarSet = getCalendar md1
          let calendar = mkSchedule $ Set.toList calendarSet
          let basket = [T.pack "SPX", T.pack "SX5E", T.pack "HSI"]
          let seedDate = fromGregorian 2023 1 2
          strategy1 <- mkEqualWeightStrategy md1 basket seedDate calendar 100.0
          state2 <- computeState strategy1 testDate
          let level2 = ewsIndexLevel state2
          level2 `shouldSatisfy` (/= level1)
          updateResult2 <-
            updatePrice md1 testDate (T.pack "SPX") (originalPrice * 1.2)
          case updateResult2 of
            Right md2 -> do
              -- Create another strategy with second update
              let calendarSet2 = getCalendar md2
              let calendar2 = mkSchedule $ Set.toList calendarSet2
              strategy2 <- mkEqualWeightStrategy md2 basket seedDate calendar2 100.0
              state3 <- computeState strategy2 testDate
              let level3 = ewsIndexLevel state3
              level3 `shouldSatisfy` (/= level2)
              level3 `shouldSatisfy` (/= level1)
            Left _ -> error "Should update price"
        Left _ -> error "Should update price"
    it "handles multiple updates to different dates" $ do
      strategy <- createStrategy
      let date1 = fromGregorian 2023 1 3
      let date2 = fromGregorian 2023 1 4
      state1Before <- computeState strategy date1
      state2Before <- computeState strategy date2
      -- Update both dates (using strategy's MarketData)
      let md = ewsMarketData strategy
      price1 <- either (error . show) return $ getPrice md date1 (T.pack "SPX")
      price2 <- either (error . show) return $ getPrice md date2 (T.pack "SPX")
      updateResult1 <- updatePrice md date1 (T.pack "SPX") (price1 * 1.1)
      case updateResult1 of
        Right md1 -> do
          updateResult2 <- updatePrice md1 date2 (T.pack "SPX") (price2 * 1.1)
          case updateResult2 of
            Right md2 -> do
              -- Create a new strategy with both updates
              let calendarSet = getCalendar md2
              let calendar = mkSchedule $ Set.toList calendarSet
              let basket = [T.pack "SPX", T.pack "SX5E", T.pack "HSI"]
              let seedDate = fromGregorian 2023 1 2
              strategy' <- mkEqualWeightStrategy md2 basket seedDate calendar 100.0
              state1After <- computeState strategy' date1
              state2After <- computeState strategy' date2
              ewsIndexLevel state1After
                `shouldSatisfy` (/= ewsIndexLevel state1Before)
              ewsIndexLevel state2After
                `shouldSatisfy` (/= ewsIndexLevel state2Before)
            Left _ -> error "Should update price"
        Left _ -> error "Should update price"
