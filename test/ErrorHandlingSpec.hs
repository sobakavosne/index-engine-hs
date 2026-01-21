{-# OPTIONS_GHC -Wno-orphans #-}

module ErrorHandlingSpec where

import           Control.Exception   (ErrorCall (..), try)
import           Data.List           (isInfixOf)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Data.Time.Calendar  (fromGregorian)
import           EqualWeightStrategy (EqualWeightStrategy,
                                      mkEqualWeightStrategy)
import           MarketData          (MarketDataError (..), getCalendar,
                                      getPrice, loadMarketData, updatePrice)
import           Runner              (getStates)
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
  describe "Error Handling" $ do
    describe "Strategy errors" $ do
      it "raises error when computing state before seed_date" $ do
        strategy <- createStrategy
        let beforeSeed = fromGregorian 2023 1 1
        result <- try $ computeState strategy beforeSeed
        case result of
          Left (ErrorCall msg) ->
            "No previous date" `shouldSatisfy` (`isInfixOf` msg)
          Right _ -> False `shouldBe` True
      it "raises error when computing state for date not in calendar" $ do
        strategy <- createStrategy
        -- Use a weekend date that's not in the calendar
        let weekendDate = fromGregorian 2023 1 7 -- Saturday
        result <- try $ computeState strategy weekendDate
        case result of
          Left (ErrorCall msg) ->
            ("DataNotFound" `isInfixOf` msg || "No data" `isInfixOf` msg)
              `shouldBe` True
          Right _ -> False `shouldBe` True
    describe "MarketData errors" $ do
      it "raises error for missing ticker" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md ->
            case getPrice md (fromGregorian 2023 1 2) (T.pack "INVALID") of
              Left (DataNotFound _ _) -> True `shouldBe` True
              Left _                  -> False `shouldBe` True
              Right _                 -> False `shouldBe` True
          Left _ -> error "Should load CSV"
      it "raises error for missing date" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md ->
            case getPrice md (fromGregorian 2020 1 1) (T.pack "SPX") of
              Left (DataNotFound _ _) -> True `shouldBe` True
              Left _                  -> False `shouldBe` True
              Right _                 -> False `shouldBe` True
          Left _ -> error "Should load CSV"
      it "raises error when updating invalid date/ticker" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            updateResult <-
              updatePrice md (fromGregorian 2020 1 1) (T.pack "SPX") 1000.0
            case updateResult of
              Left (DataNotFound _ _) -> True `shouldBe` True
              Left _                  -> False `shouldBe` True
              Right _                 -> False `shouldBe` True
          Left _ -> error "Should load CSV"
    describe "Runner errors" $ do
      it "handles dates outside range gracefully" $ do
        strategy <- createStrategy
        -- Dates before seed_date - should return empty
        states <-
          getStates
            strategy
            (Just $ fromGregorian 2022 12 1)
            (fromGregorian 2022 12 31)
        Map.size states `shouldBe` 0
