{-# OPTIONS_GHC -Wno-orphans #-}

module ScheduleSpec where

import           Control.Monad      (forM_)
import           Data.List          (nub, sort)
import qualified Data.Set           as Set
import           Data.Time.Calendar (Day, addDays, fromGregorian)
import           Schedule           (ScheduleError (..), isLastDayOfMonth,
                                     mkSchedule, next, prev, subSchedule,
                                     toList)
import           Test.Hspec         (Spec, describe, it, shouldBe)
import           Test.QuickCheck    (Arbitrary (..), Gen, Testable (property),
                                     choose, vectorOf, (===), (==>))

instance Arbitrary Day where
  arbitrary = do
    year <- choose (2000, 2100)
    month <- choose (1, 12)
    -- Generate valid day for the month
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

-- | Generate a non-empty list of dates
genDateList :: Gen [Day]
genDateList = do
  n <- choose (1, 100)
  dates <- vectorOf n arbitrary
  return $ sort $ nub dates

spec :: Spec
spec = do
  describe "mkSchedule" $ do
    it "removes duplicates"
      $ property
      $ \dates ->
          let schedule = mkSchedule dates
              uniqueDates = Set.size $ Set.fromList dates
              scheduleSize = length $ toList schedule
           in scheduleSize === uniqueDates
    it "sorts dates"
      $ property
      $ \dates ->
          let schedule = mkSchedule dates
              scheduleList = toList schedule
              sortedDates = sort $ nub dates
           in scheduleList === sortedDates
  describe "prev" $ do
    it "returns previous date when it exists"
      $ property
      $ \dates ->
          not (null dates)
            ==> let schedule = mkSchedule dates
                    sortedDates = sort $ nub dates
                    targetDate = last sortedDates
                    prevDates = init sortedDates
                 in if null prevDates
                      then case prev schedule targetDate of
                             Left _  -> property True
                             Right _ -> property False
                      else case prev schedule targetDate of
                             Left _  -> property False
                             Right d -> d === last prevDates
    it "returns error when no previous date exists"
      $ property
      $ \dates ->
          not (null dates)
            ==> let schedule = mkSchedule dates
                    sortedDates = sort $ nub dates
                    firstDate = head sortedDates
                 in case prev schedule firstDate of
                      Left _  -> property True
                      Right _ -> property False
  describe "next" $ do
    it "returns next date when it exists"
      $ property
      $ \dates ->
          not (null dates)
            ==> let schedule = mkSchedule dates
                    sortedDates = sort $ nub dates
                    targetDate = head sortedDates
                    nextDates = tail sortedDates
                 in if null nextDates
                      then case next schedule targetDate of
                             Left _  -> property True
                             Right _ -> property False
                      else case next schedule targetDate of
                             Left _  -> property False
                             Right d -> d === head nextDates
    it "returns error when no next date exists"
      $ property
      $ \dates ->
          not (null dates)
            ==> let schedule = mkSchedule dates
                    sortedDates = sort $ nub dates
                    lastDate = last sortedDates
                 in case next schedule lastDate of
                      Left _  -> property True
                      Right _ -> property False
  describe "subSchedule" $ do
    it "contains only dates in range"
      $ property
      $ \dates startDate endDate ->
          startDate
            <= endDate
                 ==> let schedule = mkSchedule dates
                         subSched = subSchedule schedule startDate endDate
                         subList = toList subSched
                      in all (\d -> d >= startDate && d <= endDate) subList
    it "contains all dates in range"
      $ property
      $ \dates startDate endDate ->
          startDate
            <= endDate
                 ==> let schedule = mkSchedule dates
                         subSched = subSchedule schedule startDate endDate
                         subList = toList subSched
                         inRangeDates =
                           filter (\d -> d >= startDate && d <= endDate)
                             $ sort
                             $ nub dates
                      in subList === inRangeDates
  describe "toList" $ do
    it "returns sorted list"
      $ property
      $ \dates ->
          let schedule = mkSchedule dates
              scheduleList = toList schedule
           in scheduleList === sort scheduleList
    it "preserves all unique dates"
      $ property
      $ \dates ->
          let schedule = mkSchedule dates
              scheduleList = toList schedule
              uniqueDates = Set.size $ Set.fromList dates
           in length scheduleList === uniqueDates
  describe "isLastDayOfMonth" $ do
    it "returns True when date is last day of month" $ do
      -- Test with specific known last days of months
      let testCases =
            [ (2023, 1, 31) -- January
            , (2023, 2, 28) -- February (non-leap)
            , (2024, 2, 29) -- February (leap)
            , (2023, 3, 31) -- March
            , (2023, 4, 30) -- April
            , (2023, 5, 31) -- May
            , (2023, 6, 30) -- June
            , (2023, 7, 31) -- July
            , (2023, 8, 31) -- August
            , (2023, 9, 30) -- September
            , (2023, 10, 31) -- October
            , (2023, 11, 30) -- November
            , (2023, 12, 31) -- December
            ]
      forM_ testCases $ \(year, month, lastDay) -> do
        let date = fromGregorian year month lastDay
        let nextDate = addDays 1 date
        let schedule = mkSchedule [date, nextDate]
        case isLastDayOfMonth schedule date of
          Left _  -> error "Should not error for valid last day of month"
          Right b -> b `shouldBe` True
    it "returns False when date is not last day of month" $ do
      let date = fromGregorian 2023 1 15
          nextDate = addDays 1 date
          schedule = mkSchedule [date, nextDate]
      case isLastDayOfMonth schedule date of
        Left _  -> False `shouldBe` True
        Right b -> b `shouldBe` False
    it "returns error when no next date exists" $ do
      -- Test with specific known last days of months (no next date in schedule)
      let testCases =
            [ (2023, 1, 31) -- January
            , (2023, 2, 28) -- February (non-leap)
            , (2024, 2, 29) -- February (leap)
            , (2023, 3, 31) -- March
            , (2023, 4, 30) -- April
            , (2023, 5, 31) -- May
            , (2023, 6, 30) -- June
            , (2023, 7, 31) -- July
            , (2023, 8, 31) -- August
            , (2023, 9, 30) -- September
            , (2023, 10, 31) -- October
            , (2023, 11, 30) -- November
            , (2023, 12, 31) -- December
            ]
      forM_ testCases $ \(year, month, lastDay) -> do
        let date = fromGregorian year month lastDay
        let schedule = mkSchedule [date] -- Only the date, no next date
        case isLastDayOfMonth schedule date of
          Left (NoNextDate _) -> True `shouldBe` True
          Left _ -> error "Should return NoNextDate error"
          Right _ -> error "Should return error when no next date exists"
    it "handles leap year February correctly" $ do
      let dates =
            [ fromGregorian 2024 2 28
            , fromGregorian 2024 2 29
            , fromGregorian 2024 3 1
            ]
          schedule = mkSchedule dates
      case isLastDayOfMonth schedule (fromGregorian 2024 2 29) of
        Left _  -> False `shouldBe` True
        Right b -> b `shouldBe` True
      case isLastDayOfMonth schedule (fromGregorian 2024 2 28) of
        Left _  -> False `shouldBe` True
        Right b -> b `shouldBe` False
    it "handles non-leap year February correctly" $ do
      let dates =
            [ fromGregorian 2023 2 27
            , fromGregorian 2023 2 28
            , fromGregorian 2023 3 1
            ]
          schedule = mkSchedule dates
      case isLastDayOfMonth schedule (fromGregorian 2023 2 28) of
        Left _  -> False `shouldBe` True
        Right b -> b `shouldBe` True
      case isLastDayOfMonth schedule (fromGregorian 2023 2 27) of
        Left _  -> False `shouldBe` True
        Right b -> b `shouldBe` False
  describe "edge cases" $ do
    it "handles empty schedule" $ do
      let schedule = mkSchedule ([] :: [Day])
      length (toList schedule) `shouldBe` 0
      case prev schedule (fromGregorian 2023 1 1) of
        Left (NoPreviousDate _) -> True `shouldBe` True
        Left (NoNextDate _)     -> True `shouldBe` True
        Right _                 -> False `shouldBe` True
      case next schedule (fromGregorian 2023 1 1) of
        Left (NoNextDate _)     -> True `shouldBe` True
        Left (NoPreviousDate _) -> True `shouldBe` True
        Right _                 -> False `shouldBe` True
    it "handles single date schedule" $ do
      let date = fromGregorian 2023 1 1
          schedule = mkSchedule [date]
      length (toList schedule) `shouldBe` 1
      case prev schedule date of
        Left (NoPreviousDate _) -> True `shouldBe` True
        Left (NoNextDate _)     -> True `shouldBe` True
        Right _                 -> False `shouldBe` True
      case next schedule date of
        Left (NoNextDate _)     -> True `shouldBe` True
        Left (NoPreviousDate _) -> True `shouldBe` True
        Right _                 -> False `shouldBe` True
      case isLastDayOfMonth schedule date of
        Left (NoNextDate _)     -> True `shouldBe` True
        Left (NoPreviousDate _) -> True `shouldBe` True
        Right _                 -> False `shouldBe` True
    it "handles subSchedule with empty range" $ do
      let dates =
            [ fromGregorian 2023 1 1
            , fromGregorian 2023 1 5
            , fromGregorian 2023 1 10
            ]
          schedule = mkSchedule dates
          subSched =
            subSchedule
              schedule
              (fromGregorian 2023 2 1)
              (fromGregorian 2023 2 28)
      length (toList subSched) `shouldBe` 0

-- | Check if a year is a leap year
isLeapYear :: Integer -> Bool
isLeapYear year =
  (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)
