{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MarketData
  ( MarketData(..)
  , MarketDataError(..)
  , loadMarketData
  , getPrice
  , getCalendar
  , updatePrice
  , getUpdatedDates
  , clearUpdatedDates
  ) where

import           Control.Exception    (Exception, IOException, try)
import           Control.Monad.STM    (STM, atomically)
import qualified Data.ByteString.Lazy as BL
import           Data.Csv             (FromNamedRecord (..), Parser,
                                       decodeByName, (.:))
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time.Calendar   (Day)
import           Data.Time.Format     (defaultTimeLocale, parseTimeM)
import qualified Data.Vector          as V
import           GHC.Conc.Sync        (TVar, newTVarIO, readTVar, writeTVar)

-- | Custom error type for MarketData operations
data MarketDataError
  = FileNotFound String
  | ParseError String
  | DataNotFound Day Text
  deriving (Show, Eq)

instance Exception MarketDataError

-- | Record type for CSV parsing
data PriceRecord = PriceRecord
  { prDate   :: Day
  , prTicker :: Text
  , prClose  :: Double
  } deriving (Show)

instance FromNamedRecord PriceRecord where
  parseNamedRecord r =
    PriceRecord
      <$> (r .: "date" >>= parseDate)
      <*> r .: "ticker"
      <*> r .: "close"
    where
      parseDate :: Text -> Parser Day
      parseDate t =
        case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
          Just d  -> return d
          Nothing -> fail $ "Invalid date format: " ++ T.unpack t

-- | Internal data structure
data MarketData = MarketData
  { mdPrices       :: Map (Day, Text) Double
  , mdUpdatedDates :: TVar (Set Day)
  , mdCalendar     :: Set Day
  }

-- | Load market data from CSV file
loadMarketData :: FilePath -> IO (Either MarketDataError MarketData)
loadMarketData file = do
  csvDataResult <- try $ BL.readFile file
  case csvDataResult of
    Left (_ :: IOException) -> return $ Left $ FileNotFound file
    Right csvData ->
      case decodeByName csvData of
        Left err -> return $ Left $ ParseError err
        Right (_, records) -> do
          let priceMap =
                Map.fromList
                  [((prDate r, prTicker r), prClose r) | r <- V.toList records]
          let calendar = Set.fromList [prDate r | r <- V.toList records]
          updatedDates <- newTVarIO Set.empty
          return $ Right $ MarketData priceMap updatedDates calendar

-- | Get price for a specific date and ticker
getPrice :: MarketData -> Day -> Text -> Either MarketDataError Double
getPrice md date ticker =
  case Map.lookup (date, ticker) (mdPrices md) of
    Just price -> Right price
    Nothing    -> Left $ DataNotFound date ticker

-- | Get all available dates (calendar)
getCalendar :: MarketData -> Set Day
getCalendar = mdCalendar

-- | Update a price (creates new MarketData with updated price)
updatePrice ::
     MarketData
  -> Day
  -> Text
  -> Double
  -> IO (Either MarketDataError MarketData)
updatePrice md date ticker newPrice = do
  -- Check if the key exists
  case Map.lookup (date, ticker) (mdPrices md) of
    Nothing -> return $ Left $ DataNotFound date ticker
    Just _ -> do
      -- Update the price map
      let newPrices = Map.insert (date, ticker) newPrice (mdPrices md)
      -- Update the updated dates set
      atomically $ do
        updated <- readTVar (mdUpdatedDates md)
        writeTVar (mdUpdatedDates md) (Set.insert date updated)
      return $ Right $ md {mdPrices = newPrices}

-- | Get set of dates that have been updated
getUpdatedDates :: MarketData -> STM (Set Day)
getUpdatedDates md = readTVar (mdUpdatedDates md)

-- | Clear the tracking of updated dates
clearUpdatedDates :: MarketData -> IO ()
clearUpdatedDates md = atomically $ writeTVar (mdUpdatedDates md) Set.empty
