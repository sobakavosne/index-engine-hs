module Runner
  ( getStates
  ) where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Time.Calendar (Day)
import           Schedule           (toList)
import           Strategy           (Strategy (..))

-- | Get the states for each date in the specified range
--
-- Args:
--   strategy: The strategy to compute
--   fromDate: Start date (Nothing means use strategy's seed date)
--   toDate: End date (inclusive)
--
-- Returns:
--   Dictionary mapping dates to their computed strategy states
getStates ::
     (Strategy s) => s -> Maybe Day -> Day -> IO (Map Day (StrategyState s))
getStates strategy fromDate toDate = do
  -- Resolve the date range using the strategy's calendar
  let schedule = resolveDates strategy fromDate toDate
  let dates = toList schedule
  -- Compute strategy state for each date in the schedule
  states <- mapM (computeState strategy) dates
  -- Return as a map
  return $ Map.fromList (zip dates states)
