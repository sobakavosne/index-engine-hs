module Schedule
  ( Schedule
  , ScheduleError(..)
  , mkSchedule
  , prev
  , next
  , subSchedule
  , isLastDayOfMonth
  , toList
  ) where

import           Control.Exception  (Exception)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Time.Calendar (Day, toGregorian)

-- | Custom error type for Schedule operations
data ScheduleError
  = NoPreviousDate Day
  | NoNextDate Day
  deriving (Show, Eq)

instance Exception ScheduleError

-- | Schedule wraps a sorted set of dates
newtype Schedule =
  Schedule (Set Day)
  deriving (Show, Eq)

-- | Create a schedule from a list of dates (sorts and removes duplicates)
mkSchedule :: [Day] -> Schedule
mkSchedule = Schedule . Set.fromList

-- | Get the previous date before the given date
prev :: Schedule -> Day -> Either ScheduleError Day
prev (Schedule dates) targetDate =
  case Set.lookupLT targetDate dates of
    Just d  -> Right d
    Nothing -> Left $ NoPreviousDate targetDate

-- | Get the next date after the given date
next :: Schedule -> Day -> Either ScheduleError Day
next (Schedule dates) targetDate =
  case Set.lookupGT targetDate dates of
    Just d  -> Right d
    Nothing -> Left $ NoNextDate targetDate

-- | Create a sub-schedule with dates within the given range (inclusive)
subSchedule :: Schedule -> Day -> Day -> Schedule
subSchedule (Schedule dates) startDate endDate =
  Schedule $ Set.filter (\d -> d >= startDate && d <= endDate) dates

-- | Check if target_date is the last day of the month in this schedule
isLastDayOfMonth :: Schedule -> Day -> Either ScheduleError Bool
isLastDayOfMonth sched targetDate = do
  nextDate <- next sched targetDate
  let (targetYear, targetMonth, _) = toGregorian targetDate
  let (nextYear, nextMonth, _) = toGregorian nextDate
  return $ targetMonth /= nextMonth || targetYear /= nextYear

-- | Convert schedule to a sorted list
toList :: Schedule -> [Day]
toList (Schedule dates) = Set.toAscList dates
