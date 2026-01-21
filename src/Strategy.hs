{-# LANGUAGE TypeFamilies #-}

module Strategy
  ( Strategy(..)
  ) where

import           Data.Kind          (Type)
import           Data.Time.Calendar (Day)
import           Schedule           (Schedule)

-- | Type class for financial index computation strategies
--
-- This class defines the interface that all index strategies must implement.
-- It uses an associated type to allow different strategies to return
-- different state types while maintaining type safety.
class Strategy s where
  -- | The type of state returned by this strategy
  type StrategyState s :: Type
  -- | Resolve a date range into a schedule of valid computation dates
  --
  -- This method determines which dates within the given range should be
  -- included in strategy calculations, typically filtering for business days
  -- or other relevant trading dates.
  resolveDates :: s -> Maybe Day -> Day -> Schedule
  -- | Compute the state for a specific date
  --
  -- This method calculates all relevant formulae for the index strategy
  -- on the given date, including the index level, and any
  -- other strategy-specific state information.
  computeState :: s -> Day -> IO (StrategyState s)
