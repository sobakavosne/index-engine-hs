module StateStore
  ( StateStore
  , mkStateStore
  , getState
  , putState
  , invalidateFromDate
  , clear
  ) where

import           Control.Monad.STM  (STM, atomically)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)
import           GHC.Conc.Sync      (TVar, newTVarIO, readTVar, writeTVar)

-- | Type alias for dependencies: set of (date, ticker) pairs
type Dependencies = Set (Day, Text)

-- | Internal state of the StateStore
data StateStoreState s = StateStoreState
  { sssCache        :: Map Day s
  , sssDependencies :: Map Day Dependencies
  }

-- | StateStore provides thread-safe caching with dependency tracking
data StateStore s = StateStore
  { ssState           :: TVar (StateStoreState s)
  , ssGetUpdatedDates :: STM (Set Day) -- Function to get updated dates from MarketData
  }

-- | Create a new StateStore
mkStateStore :: STM (Set Day) -> IO (StateStore s)
mkStateStore getUpdatedDates = do
  state <- newTVarIO $ StateStoreState Map.empty Map.empty
  return $ StateStore state getUpdatedDates

-- | Get a cached state if it exists and is valid
getState :: StateStore s -> Day -> STM (Maybe s)
getState ss targetDate = do
  state <- readTVar (ssState ss)
  case Map.lookup targetDate (sssCache state) of
    Nothing -> return Nothing
    Just cachedState -> do
      -- Check if all dependencies are still valid
      isValid <- checkValid ss state targetDate
      if isValid
        then return $ Just cachedState
        else do
          -- Invalidate this state
          let newCache = Map.delete targetDate (sssCache state)
          let newDeps = Map.delete targetDate (sssDependencies state)
          writeTVar (ssState ss) $ StateStoreState newCache newDeps
          return Nothing

-- | Check if a cached state is still valid
checkValid :: StateStore s -> StateStoreState s -> Day -> STM Bool
checkValid ss state targetDate =
  case Map.lookup targetDate (sssDependencies state) of
    Nothing -> return False
    Just deps -> do
      updatedDates <- ssGetUpdatedDates ss
      let dependencyDates = Set.map fst deps
      -- State is valid if no dependency dates have been updated
      return $ Set.null (Set.intersection dependencyDates updatedDates)

-- | Store a state with its dependencies
putState :: StateStore s -> Day -> s -> Dependencies -> STM ()
putState ss targetDate state deps = do
  s <- readTVar (ssState ss)
  let newCache = Map.insert targetDate state (sssCache s)
  let newDeps = Map.insert targetDate deps (sssDependencies s)
  writeTVar (ssState ss) $ StateStoreState newCache newDeps

-- | Invalidate all states that depend on market data at or after the given date
-- Per the spec: when market data at date X changes, all states at date >= X
-- must be invalidated because they may depend on it.
invalidateFromDate :: StateStore s -> Day -> STM ()
invalidateFromDate ss invalidatedDate = do
  s <- readTVar (ssState ss)
  let datesToRemove =
        Map.keys $ Map.filterWithKey (\d _ -> d >= invalidatedDate) (sssCache s)
  let newCache = foldr Map.delete (sssCache s) datesToRemove
  let newDeps = foldr Map.delete (sssDependencies s) datesToRemove
  writeTVar (ssState ss) $ StateStoreState newCache newDeps

-- | Clear all cached states
clear :: StateStore s -> IO ()
clear ss =
  atomically $ do
    writeTVar (ssState ss) $ StateStoreState Map.empty Map.empty
