module Main where

import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Data.Time.Calendar  (Day, fromGregorian)
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           EqualWeightStrategy (EqualWeightStrategyState (..),
                                      mkEqualWeightStrategy)
import           MarketData          (getCalendar, loadMarketData)
import           Runner              (getStates)
import           Schedule            (mkSchedule)
import           System.IO           ()

-- | Format Day as YYYY-MM-DD string
formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"

-- | Write CSV file manually (simple two-column format)
writeCSV :: FilePath -> [(Day, Double)] -> IO ()
writeCSV filePath records = do
  let header = "date,index_level\n"
  let rows =
        map
          (\(date, level) -> formatDay date ++ "," ++ show level ++ "\n")
          records
  writeFile filePath $ concat (header : rows)

main :: IO ()
main = do
  putStrLn "Index Engine - Haskell Implementation"
  putStrLn "====================================="
  -- Load market data from CSV
  putStrLn "\nLoading market data..."
  result <- loadMarketData "data/sample_prices.csv"
  case result of
    Left err -> do
      putStrLn $ "Error loading market data: " ++ show err
      putStrLn "Please ensure data/sample_prices.csv exists"
    Right md -> do
      putStrLn "Market data loaded successfully"
      -- Get calendar from market data
      let calendarSet = getCalendar md
      let calendar = mkSchedule $ Set.toList calendarSet
      -- Create equal weight strategy
      let basket = [T.pack "SPX", T.pack "SX5E", T.pack "HSI"]
      let seedDate = fromGregorian 2023 1 2
      let initialLevel = 100.0
      putStrLn "\nCreating equal weight strategy..."
      strategy <- mkEqualWeightStrategy md basket seedDate calendar initialLevel
      putStrLn "Strategy created"
      -- Compute states for a date range (matching expected_output.csv)
      let fromDate = Just seedDate
      let toDate = fromGregorian 2023 6 29
      putStrLn
        $ "\nComputing states from "
            ++ show (fromMaybe seedDate fromDate)
            ++ " to "
            ++ show toDate
      states <- getStates strategy fromDate toDate
      -- Convert to list of (date, index_level) pairs, sorted by date
      let outputRecords = Map.toAscList $ Map.map ewsIndexLevel states
      -- Write to CSV file
      writeCSV "data/sample_output.csv" outputRecords
      putStrLn $ "\nComputed " ++ show (length outputRecords) ++ " states"
      putStrLn "Output written to data/sample_output.csv"
      putStrLn "Done!"
