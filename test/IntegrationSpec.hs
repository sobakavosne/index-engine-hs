{-# OPTIONS_GHC -Wno-orphans #-}

module IntegrationSpec where

import           Control.Monad       (forM_)
import           Data.List           (sort)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (listToMaybe, mapMaybe)
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Data.Time.Calendar  (Day, fromGregorian)
import           EqualWeightStrategy (EqualWeightStrategy,
                                      EqualWeightStrategyState (..),
                                      mkEqualWeightStrategy)
import           MarketData          (getCalendar, loadMarketData)
import           Runner              (getStates)
import           Schedule            (mkSchedule)
import           Test.Hspec          (Spec, describe, it, shouldBe)

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

-- | Parse CSV line to extract date and index_level
parseCSVLine :: String -> Maybe (Day, Double)
parseCSVLine line =
  case words
         $ map
             (\c ->
                if c == ','
                  then ' '
                  else c)
             line of
    [dateStr, levelStr] ->
      case (parseDate dateStr, readMaybe levelStr) of
        (Just date, Just level) -> Just (date, level)
        _                       -> Nothing
    _ -> Nothing
  where
    parseDate :: String -> Maybe Day
    parseDate s =
      case words s of
        [dateStr] -> readMaybe dateStr
        _         -> Nothing
    readMaybe :: Read a => String -> Maybe a
    readMaybe = fmap fst . listToMaybe . reads

spec :: Spec
spec = do
  describe "Integration" $ do
    it "getStates matches expected_output.csv" $ do
      strategy <- createStrategy
      states <-
        getStates
          strategy
          (Just $ fromGregorian 2023 1 2)
          (fromGregorian 2023 6 29)
      -- Read expected output
      expectedContent <- readFile "data/expected_output.csv"
      let expectedLines = drop 1 $ lines expectedContent -- Skip header
      let expectedData = mapMaybe parseCSVLine expectedLines
      -- Compare
      Map.size states `shouldBe` length expectedData
      forM_ expectedData $ \(date, expectedLevel) ->
        case Map.lookup date states of
          Just state ->
            abs (ewsIndexLevel state - expectedLevel) < 1e-6 `shouldBe` True
          Nothing -> error $ "Missing date in computed states: " ++ show date
    it "produces output with correct format" $ do
      strategy <- createStrategy
      states <-
        getStates
          strategy
          (Just $ fromGregorian 2023 1 2)
          (fromGregorian 2023 6 29)
      -- Verify all states have valid structure
      all
        (\state ->
           ewsIndexLevel state > 0
             && abs (sum (Map.elems $ ewsWeights state) - 1.0) < 1e-6
             && Map.size (ewsReturns state) > 0)
        (Map.elems states)
        `shouldBe` True
    it "produces output for all expected dates" $ do
      strategy <- createStrategy
      states <-
        getStates
          strategy
          (Just $ fromGregorian 2023 1 2)
          (fromGregorian 2023 6 29)
      -- Read expected output to get date list
      expectedContent <- readFile "data/expected_output.csv"
      let expectedLines = drop 1 $ lines expectedContent
      let expectedDates = mapMaybe (fmap fst . parseCSVLine) expectedLines
      -- All expected dates should be in computed states
      all (`Map.member` states) expectedDates `shouldBe` True
      -- No extra dates (within the range)
      let computedDates = Map.keys states
      let expectedDateSet = Set.fromList expectedDates
      let computedDateSet = Set.fromList computedDates
      Set.size (computedDateSet `Set.difference` expectedDateSet) `shouldBe` 0
    it "produces chronologically ordered output" $ do
      strategy <- createStrategy
      states <-
        getStates
          strategy
          (Just $ fromGregorian 2023 1 2)
          (fromGregorian 2023 6 29)
      let dates = Map.keys states
      dates `shouldBe` sort dates
