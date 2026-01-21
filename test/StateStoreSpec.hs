{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StateStoreSpec where

import           Control.Monad      (forM_)
import           Control.Monad.STM  (STM, atomically)
import           Data.Maybe         (isNothing)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import qualified Data.Text          as T
import           Data.Time.Calendar (Day, fromGregorian, isLeapYear)
import           GHC.Conc.Sync      (TVar, newTVarIO, readTVar, writeTVar)
import           StateStore         (StateStore, clear, getState,
                                     invalidateFromDate, mkStateStore, putState)
import           Test.Hspec         (Spec, describe, it, shouldBe)
import           Test.QuickCheck    (Arbitrary (..), Testable (property),
                                     choose, ioProperty, listOf, (===), (==>))

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

-- | Generate Text
instance Arbitrary T.Text where
  arbitrary = T.pack <$> listOf (choose ('a', 'z'))
  shrink = map T.pack . shrink . T.unpack

-- | Generate a simple integer state for testing
type TestState = Int

-- | Mock function to get updated dates (always returns empty set for testing)
mockGetUpdatedDates :: STM (Set Day)
mockGetUpdatedDates = return Set.empty

-- | Create a mock getUpdatedDates that uses a TVar for dynamic updates
mkMockGetUpdatedDates :: IO (TVar (Set Day), STM (Set Day))
mkMockGetUpdatedDates = do
  tv <- newTVarIO Set.empty
  return (tv, readTVar tv)

spec :: Spec
spec = do
  describe "StateStore" $ do
    it "getState returns Nothing for non-existent state"
      $ property
      $ \date ->
          ioProperty $ do
            store <-
              mkStateStore mockGetUpdatedDates :: IO (StateStore TestState)
            result <- atomically $ getState store date
            return $ result === (Nothing :: Maybe TestState)
    it "putState and getState round-trip"
      $ property
      $ \date (state :: TestState) (deps :: Set (Day, T.Text)) ->
          ioProperty $ do
            store <- mkStateStore mockGetUpdatedDates
            atomically $ putState store date state deps
            result <- atomically $ getState store date
            return $ result === Just state
    it "invalidateFromDate removes states at or after date"
      $ property
      $ \(dates :: [Day]) invalidDate ->
          not (null dates) ==> ioProperty $ do
            store <- mkStateStore mockGetUpdatedDates
          -- Put states for all dates
            forM_ dates $ \date ->
              atomically $ putState store date (0 :: TestState) Set.empty
          -- Invalidate from invalidDate
            atomically $ invalidateFromDate store invalidDate
          -- Check that all dates >= invalidDate are removed
            results <- mapM (atomically . getState store) dates
            let shouldBeRemoved = filter (>= invalidDate) dates
            let shouldRemain = filter (< invalidDate) dates
            let removedResults =
                  [ r
                  | (date, r) <- zip dates results
                  , date `elem` shouldBeRemoved
                  ]
            let remainingResults =
                  [r | (date, r) <- zip dates results, date `elem` shouldRemain]
            return
              $ all (== Nothing) removedResults
                  && notElem Nothing remainingResults
    it "clear removes all states"
      $ property
      $ \(dates :: [Day]) ->
          ioProperty $ do
            store <- mkStateStore mockGetUpdatedDates
        -- Put states for all dates
            forM_ dates $ \date ->
              atomically $ putState store date (0 :: TestState) Set.empty
        -- Clear the store
            clear store
        -- Check that all states are removed
            results <- mapM (atomically . getState store) dates
            return $ all (== Nothing) results
    it "multiple puts overwrite previous state"
      $ property
      $ \date (state1 :: TestState) (state2 :: TestState) ->
          state1 /= state2 ==> ioProperty $ do
            store <- mkStateStore mockGetUpdatedDates
            atomically $ putState store date state1 Set.empty
            atomically $ putState store date state2 Set.empty
            result <- atomically $ getState store date
            return $ result === Just state2
    it "invalidates state when dependencies are updated"
      $ property
      $ \date (ticker :: T.Text) ->
          ioProperty $ do
            (updatedDatesTV, getUpdatedDates) <- mkMockGetUpdatedDates
            store <- mkStateStore getUpdatedDates
            let deps = Set.singleton (date, ticker)
            atomically $ putState store date (0 :: TestState) deps
            -- Verify state is cached
            result1 <- atomically $ getState store date
            -- Update the dependency date
            atomically $ do
              current <- readTVar updatedDatesTV
              writeTVar updatedDatesTV $ Set.insert date current
            -- State should now be invalid
            result2 <- atomically $ getState store date
            return $ (result1 == Just (0 :: TestState)) && isNothing result2
    it "keeps state valid when unrelated dates are updated"
      $ property
      $ \date1 date2 (ticker :: T.Text) ->
          date1 /= date2 ==> ioProperty $ do
            (updatedDatesTV, getUpdatedDates) <- mkMockGetUpdatedDates
            store <- mkStateStore getUpdatedDates
            let deps = Set.singleton (date1, ticker)
            atomically $ putState store date1 (0 :: TestState) deps
            -- Update an unrelated date
            atomically $ do
              current <- readTVar updatedDatesTV
              writeTVar updatedDatesTV $ Set.insert date2 current
            -- State should still be valid
            result <- atomically $ getState store date1
            return $ result == Just (0 :: TestState)
    it "invalidates state when previous date dependency is updated"
      $ property
      $ \prevDate date (ticker :: T.Text) ->
          prevDate < date ==> ioProperty $ do
            (updatedDatesTV, getUpdatedDates) <- mkMockGetUpdatedDates
            store <- mkStateStore getUpdatedDates
            -- State at date depends on prevDate
            let deps = Set.fromList [(prevDate, ticker), (date, ticker)]
            atomically $ putState store date (0 :: TestState) deps
            -- Update prevDate (a dependency)
            atomically $ do
              current <- readTVar updatedDatesTV
              writeTVar updatedDatesTV $ Set.insert prevDate current
            -- State should be invalid
            result <- atomically $ getState store date
            return $ isNothing result
    it "handles multiple states with overlapping dependencies"
      $ property
      $ \date1 date2 (ticker :: T.Text) ->
          date1 /= date2 ==> ioProperty $ do
            (updatedDatesTV, getUpdatedDates) <- mkMockGetUpdatedDates
            store <- mkStateStore getUpdatedDates
            -- Both states depend on date1
            atomically $ do
              putState
                store
                date1
                (1 :: TestState)
                (Set.singleton (date1, ticker))
              putState
                store
                date2
                (2 :: TestState)
                (Set.fromList [(date1, ticker), (date2, ticker)])
            -- Update date1 - both should be invalid
            atomically $ do
              current <- readTVar updatedDatesTV
              writeTVar updatedDatesTV $ Set.insert date1 current
            result1 <- atomically $ getState store date1
            result2 <- atomically $ getState store date2
            return $ isNothing result1 && isNothing result2
    it "handles empty dependencies correctly"
      $ property
      $ \date unrelatedDate (_ticker :: T.Text) ->
          date /= unrelatedDate ==> ioProperty $ do
            (updatedDatesTV, getUpdatedDates) <- mkMockGetUpdatedDates
            store <- mkStateStore getUpdatedDates
            -- Store with empty dependencies (like seed date)
            atomically $ putState store date (0 :: TestState) Set.empty
            -- Update unrelated date
            atomically $ do
              current <- readTVar updatedDatesTV
              writeTVar updatedDatesTV $ Set.insert unrelatedDate current
            -- State should still be valid (no dependencies)
            result <- atomically $ getState store date
            return $ result == Just (0 :: TestState)
    it "handles multiple dependencies correctly"
      $ property
      $ \date (ticker1 :: T.Text) (ticker2 :: T.Text) ->
          ticker1 /= ticker2 ==> ioProperty $ do
            (updatedDatesTV, getUpdatedDates) <- mkMockGetUpdatedDates
            store <- mkStateStore getUpdatedDates
            let deps = Set.fromList [(date, ticker1), (date, ticker2)]
            atomically $ putState store date (0 :: TestState) deps
            -- Update one dependency
            atomically $ do
              current <- readTVar updatedDatesTV
              writeTVar updatedDatesTV $ Set.insert date current
            -- State should be invalid
            result <- atomically $ getState store date
            return $ isNothing result
  describe "StateStore edge cases" $ do
    it "handles empty dependencies" $ do
      store <- mkStateStore mockGetUpdatedDates
      let date = fromGregorian 2023 1 2
      atomically $ putState store date (42 :: TestState) Set.empty
      result <- atomically $ getState store date
      result `shouldBe` Just 42
