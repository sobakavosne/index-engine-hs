{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MarketDataSpec where

import           Control.Exception  (IOException, try)
import           Control.Monad.STM  (atomically)
import           Data.List          (sort)
import qualified Data.Set           as Set
import qualified Data.Text          as T
import           Data.Time.Calendar (Day, fromGregorian)
import           MarketData         (MarketDataError (..), clearUpdatedDates,
                                     getCalendar, getPrice, getUpdatedDates,
                                     loadMarketData, updatePrice)
import           System.Directory   (removeFile)
import           Test.Hspec         (Spec, describe, it, shouldBe,
                                     shouldSatisfy)
import           Test.QuickCheck    (Arbitrary (..), choose, listOf)

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

-- | Generate Text
instance Arbitrary T.Text where
  arbitrary = T.pack <$> listOf (choose ('A', 'Z'))
  shrink = map T.pack . shrink . T.unpack

-- | Check if a year is a leap year
isLeapYear :: Integer -> Bool
isLeapYear year =
  (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

spec :: Spec
spec = do
  describe "MarketData" $ do
    describe "loadMarketData" $ do
      it "loads valid CSV file" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            price <-
              either (error . show) return
                $ getPrice md (fromGregorian 2023 1 2) (T.pack "SPX")
            price `shouldSatisfy` (> 0)
          Left _ -> error "Should not fail"
      it "returns error for nonexistent file" $ do
        result <- try $ loadMarketData "nonexistent_file.csv"
        case result of
          Left (_ :: IOException)       -> True `shouldBe` True -- File not found exception
          Right (Left (FileNotFound _)) -> True `shouldBe` True
          Right (Left _)                -> False `shouldBe` True
          Right (Right _)               -> False `shouldBe` True
      it "returns error for invalid CSV" $ do
        -- Create invalid CSV
        let tempFile = "test_invalid.csv"
        writeFile tempFile "invalid,data\nnot,proper,format\n"
        result <- loadMarketData tempFile
        removeFile tempFile
        case result of
          Left (ParseError _) -> True `shouldBe` True
          Left _              -> False `shouldBe` True
          Right _             -> False `shouldBe` True
      it "handles empty CSV file" $ do
        let tempFile = "test_empty.csv"
        writeFile tempFile "date,ticker,close\n"
        result <- loadMarketData tempFile
        removeFile tempFile
        case result of
          Right md -> do
            calendar <- return $ getCalendar md
            Set.size calendar `shouldBe` 0
          Left _ -> error "Should load empty CSV"
      it "handles single row CSV" $ do
        let tempFile = "test_single.csv"
        writeFile tempFile "date,ticker,close\n2023-01-02,SPX,1000.0\n"
        result <- loadMarketData tempFile
        removeFile tempFile
        case result of
          Right md -> do
            price <-
              either (error . show) return
                $ getPrice md (fromGregorian 2023 1 2) (T.pack "SPX")
            price `shouldBe` 1000.0
            let calendar = getCalendar md
            Set.size calendar `shouldBe` 1
          Left _ -> error "Should load single row CSV"
    describe "getPrice" $ do
      it "gets valid price for date and ticker" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md ->
            case getPrice md (fromGregorian 2023 1 2) (T.pack "SPX") of
              Right price -> price `shouldSatisfy` (> 0)
              Left _      -> False `shouldBe` True
          Left _ -> error "Should load CSV"
      it "gets different prices for different tickers" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            spx <-
              either (error . show) return
                $ getPrice md (fromGregorian 2023 1 2) (T.pack "SPX")
            sx5e <-
              either (error . show) return
                $ getPrice md (fromGregorian 2023 1 2) (T.pack "SX5E")
            hsi <-
              either (error . show) return
                $ getPrice md (fromGregorian 2023 1 2) (T.pack "HSI")
            spx `shouldSatisfy` (> 0)
            sx5e `shouldSatisfy` (> 0)
            hsi `shouldSatisfy` (> 0)
            spx `shouldSatisfy` (/= sx5e)
            sx5e `shouldSatisfy` (/= hsi)
          Left _ -> error "Should load CSV"
      it "returns error for invalid date" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md ->
            case getPrice md (fromGregorian 2020 1 1) (T.pack "SPX") of
              Left (DataNotFound _ _) -> True `shouldBe` True
              Left _                  -> False `shouldBe` True
              Right _                 -> False `shouldBe` True
          Left _ -> error "Should load CSV"
      it "returns error for invalid ticker" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md ->
            case getPrice md (fromGregorian 2023 1 2) (T.pack "INVALID") of
              Left (DataNotFound _ _) -> True `shouldBe` True
              Left _                  -> False `shouldBe` True
              Right _                 -> False `shouldBe` True
          Left _ -> error "Should load CSV"
    describe "getCalendar" $ do
      it "returns sorted dates" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            let calendar = getCalendar md
            let dates = Set.toAscList calendar
            dates `shouldBe` sort dates
          Left _ -> error "Should load CSV"
      it "returns non-empty calendar for valid data" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            let calendar = getCalendar md
            Set.size calendar `shouldSatisfy` (> 0)
          Left _ -> error "Should load CSV"
    describe "updatePrice" $ do
      it "updates price in memory" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            let newPrice = 5000.0
            originalPrice <-
              either (error . show) return
                $ getPrice md (fromGregorian 2023 1 2) (T.pack "SPX")
            updateResult <-
              updatePrice md (fromGregorian 2023 1 2) (T.pack "SPX") newPrice
            case updateResult of
              Right md' -> do
                updatedPrice <-
                  either (error . show) return
                    $ getPrice md' (fromGregorian 2023 1 2) (T.pack "SPX")
                updatedPrice `shouldBe` newPrice
                updatedPrice `shouldSatisfy` (/= originalPrice)
              Left _ -> False `shouldBe` True
          Left _ -> error "Should load CSV"
      it "updates multiple prices" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            updateResult1 <-
              updatePrice md (fromGregorian 2023 1 2) (T.pack "SPX") 5000.0
            case updateResult1 of
              Right md1 -> do
                updateResult2 <-
                  updatePrice
                    md1
                    (fromGregorian 2023 1 2)
                    (T.pack "SX5E")
                    6000.0
                case updateResult2 of
                  Right md2 -> do
                    updateResult3 <-
                      updatePrice
                        md2
                        (fromGregorian 2023 1 3)
                        (T.pack "HSI")
                        7000.0
                    case updateResult3 of
                      Right md3 -> do
                        spx <-
                          either (error . show) return
                            $ getPrice
                                md3
                                (fromGregorian 2023 1 2)
                                (T.pack "SPX")
                        sx5e <-
                          either (error . show) return
                            $ getPrice
                                md3
                                (fromGregorian 2023 1 2)
                                (T.pack "SX5E")
                        hsi <-
                          either (error . show) return
                            $ getPrice
                                md3
                                (fromGregorian 2023 1 3)
                                (T.pack "HSI")
                        spx `shouldBe` 5000.0
                        sx5e `shouldBe` 6000.0
                        hsi `shouldBe` 7000.0
                      Left _ -> error "Should update price"
                  Left _ -> error "Should update price"
              Left _ -> error "Should update price"
          Left _ -> error "Should load CSV"
      it "returns error for invalid date/ticker" $ do
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
    describe "getUpdatedDates" $ do
      it "returns empty set initially" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            updated <- atomically $ getUpdatedDates md
            Set.size updated `shouldBe` 0
          Left _ -> error "Should load CSV"
      it "tracks updated dates" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            -- Update a known date
            updateResult <-
              updatePrice md (fromGregorian 2023 1 2) (T.pack "SPX") 5000.0
            case updateResult of
              Right md' -> do
                updated <- atomically $ getUpdatedDates md'
                -- Date should be in updated set
                Set.member (fromGregorian 2023 1 2) updated `shouldBe` True
              Left _ -> error "Should update price"
          Left _ -> error "Should load CSV"
      it "tracks multiple updated dates" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            updateResult1 <-
              updatePrice md (fromGregorian 2023 1 2) (T.pack "SPX") 5000.0
            case updateResult1 of
              Right md1 -> do
                updated1 <- atomically $ getUpdatedDates md1
                Set.member (fromGregorian 2023 1 2) updated1 `shouldBe` True
                updateResult2 <-
                  updatePrice
                    md1
                    (fromGregorian 2023 1 3)
                    (T.pack "SX5E")
                    6000.0
                case updateResult2 of
                  Right md2 -> do
                    updated2 <- atomically $ getUpdatedDates md2
                    Set.member (fromGregorian 2023 1 2) updated2 `shouldBe` True
                    Set.member (fromGregorian 2023 1 3) updated2 `shouldBe` True
                    Set.size updated2 `shouldBe` 2
                  Left _ -> error "Should update price"
              Left _ -> error "Should update price"
          Left _ -> error "Should load CSV"
    describe "clearUpdatedDates" $ do
      it "clears updated dates tracking" $ do
        result <- loadMarketData "data/sample_prices.csv"
        case result of
          Right md -> do
            updateResult <-
              updatePrice md (fromGregorian 2023 1 2) (T.pack "SPX") 5000.0
            case updateResult of
              Right md' -> do
                updated1 <- atomically $ getUpdatedDates md'
                Set.size updated1 `shouldBe` 1
                clearUpdatedDates md'
                updated2 <- atomically $ getUpdatedDates md'
                Set.size updated2 `shouldBe` 0
                -- Price should still be updated
                price <-
                  either (error . show) return
                    $ getPrice md' (fromGregorian 2023 1 2) (T.pack "SPX")
                price `shouldBe` 5000.0
              Left _ -> error "Should update price"
          Left _ -> error "Should load CSV"
