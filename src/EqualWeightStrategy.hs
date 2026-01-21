{-# LANGUAGE TypeFamilies #-}

module EqualWeightStrategy
  ( EqualWeightStrategyState(..)
  , EqualWeightStrategy(..)
  , mkEqualWeightStrategy
  ) where

import           Control.Monad.STM  (atomically)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)
import qualified Data.Set           as Set
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)
import           MarketData         (MarketData, getPrice, getUpdatedDates)
import           Schedule           (Schedule, isLastDayOfMonth, prev,
                                     subSchedule)
import           StateStore         (StateStore, getState, mkStateStore,
                                     putState)
import           Strategy           (Strategy (..))

-- | Represents the state of an equal weight strategy at a specific point in time
data EqualWeightStrategyState = EqualWeightStrategyState
  { ewsReturns         :: Map Text Double -- Returns for each asset
  , ewsPortfolioReturn :: Double -- Overall portfolio return
  , ewsIndexLevel      :: Double -- Current index level
  , ewsWeights         :: Map Text Double -- Current portfolio weights
  } deriving (Show, Eq)

-- | An equal weight index strategy that rebalances monthly
data EqualWeightStrategy = EqualWeightStrategy
  { ewsMarketData        :: MarketData
  , ewsBasket            :: [Text]
  , ewsSeedDate          :: Day
  , ewsCalendar          :: Schedule
  , ewsInitialIndexLevel :: Double
  , ewsStateStore        :: StateStore EqualWeightStrategyState
  }

instance Strategy EqualWeightStrategy where
  type StrategyState EqualWeightStrategy = EqualWeightStrategyState
  resolveDates strategy fromDate toDate =
    let startDate = fromMaybe (ewsSeedDate strategy) fromDate
     in subSchedule (ewsCalendar strategy) startDate toDate
  computeState strategy targetDate = do
    -- Check cache atomically
    cached <- atomically $ getState (ewsStateStore strategy) targetDate
    case cached of
      Just state -> return state
      Nothing    -> computeStateUnsafe strategy targetDate

-- | Create a new EqualWeightStrategy
mkEqualWeightStrategy ::
     MarketData -> [Text] -> Day -> Schedule -> Double -> IO EqualWeightStrategy
mkEqualWeightStrategy md basket seedDate calendar initialLevel = do
  stateStore <- mkStateStore (getUpdatedDates md)
  return
    $ EqualWeightStrategy md basket seedDate calendar initialLevel stateStore

-- | Internal computation without STM (called after cache check)
computeStateUnsafe :: EqualWeightStrategy -> Day -> IO EqualWeightStrategyState
computeStateUnsafe strategy targetDate
  | targetDate == ewsSeedDate strategy = do
      -- Base case: return initial state at seed date
    let state =
          EqualWeightStrategyState
            { ewsReturns =
                Map.fromList [(asset, 0.0) | asset <- ewsBasket strategy]
            , ewsPortfolioReturn = 0.0
            , ewsIndexLevel = ewsInitialIndexLevel strategy
            , ewsWeights =
                Map.fromList
                  [ (asset, 1.0 / fromIntegral (length (ewsBasket strategy)))
                  | asset <- ewsBasket strategy
                  ]
            }
    let deps = Set.empty -- Seed date doesn't depend on market data
    atomically $ putState (ewsStateStore strategy) targetDate state deps
    return state
  | otherwise = do
      -- Incremental case: compute based on previous day
    prevDate <-
      case prev (ewsCalendar strategy) targetDate of
        Left _  -> error $ "No previous date for " ++ show targetDate
        Right d -> return d
      -- Recursive call to get previous state
    prevState <- computeState strategy prevDate
      -- Calculate daily returns for each asset
    returns <-
      Map.fromList
        <$> mapM
              (computeReturn strategy targetDate prevDate)
              (ewsBasket strategy)
      -- Calculate portfolio return as weighted sum of asset returns
    let portfolioReturn =
          sum
            [ returns Map.! asset * (ewsWeights prevState Map.! asset)
            | asset <- ewsBasket strategy
            ]
    let indexLevel = ewsIndexLevel prevState * (1 + portfolioReturn)
      -- Rebalance weights at end of month, otherwise let them drift
    isLastDay <-
      case isLastDayOfMonth (ewsCalendar strategy) targetDate of
        Left _  -> return False
        Right b -> return b
    let weights =
          if isLastDay
            then Map.fromList
                   [ (asset, 1.0 / fromIntegral (length (ewsBasket strategy)))
                   | asset <- ewsBasket strategy
                   ]
            else Map.fromList
                   [ ( asset
                     , ewsWeights prevState Map.! asset
                         * (1 + returns Map.! asset)
                         / (1 + portfolioReturn))
                   | asset <- ewsBasket strategy
                   ]
    let state =
          EqualWeightStrategyState
            { ewsReturns = returns
            , ewsPortfolioReturn = portfolioReturn
            , ewsIndexLevel = indexLevel
            , ewsWeights = weights
            }
      -- Track dependencies: state at date depends on market data at date and prev_date
    let deps =
          Set.fromList [(targetDate, asset) | asset <- ewsBasket strategy]
            `Set.union` Set.fromList
                          [(prevDate, asset) | asset <- ewsBasket strategy]
    atomically $ putState (ewsStateStore strategy) targetDate state deps
    return state

-- | Compute return for a single asset
computeReturn :: EqualWeightStrategy -> Day -> Day -> Text -> IO (Text, Double)
computeReturn strategy date prevDate asset = do
  todayPrice <-
    case getPrice (ewsMarketData strategy) date asset of
      Left err -> error $ show err
      Right p  -> return p
  prevPrice <-
    case getPrice (ewsMarketData strategy) prevDate asset of
      Left err -> error $ show err
      Right p  -> return p
  return (asset, todayPrice / prevPrice - 1.0)
