-- By Reddit|Github user chessai:
-- https://www.reddit.com/r/haskell/comments/a50xpr/datahaskell_solve_this_small_problem_to_fill_some/ebn7r5n/
-- code copied from here:
-- https://gist.github.com/chessai/e5a04ddcbc6c6708333e187ee8ae41a3#file-main-hs-L9

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib where

import Colonnade (Headed)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Siphon (Siphon, SiphonError)
import Streaming (Stream, Of)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Siphon as Siphon
import qualified Streaming.Prelude as S

main :: IO ()
main = do
  (prices, purchases) <- readCSVs 
  s <- doStuff prices purchases
  putStrLn . show $ s

doStuff :: forall m. Monad m
  => Stream (Of Price) m (Maybe SiphonError)
  -> Stream (Of Purchase) m (Maybe SiphonError)
  -> m (Map Text Int, Double)
doStuff prices purchases = do
  priceMapping <- S.foldMap_ (\(Price i p) -> Map.singleton (Text.strip i) p) prices
  let mapWithPrice :: Purchase -> Maybe (Purchase, Int)
      mapWithPrice purch = (\pric -> (purch, pric * unitsBought purch)) <$> Map.lookup (Text.strip $ itemBought purch) priceMapping
  let pricedPurchases :: Stream (Of (Purchase, Int)) m (Maybe SiphonError)
      pricedPurchases = S.mapMaybe mapWithPrice purchases
  let nonLegalPurchases :: Stream (Of (Purchase, Int)) m (Maybe SiphonError)
      nonLegalPurchases = S.filter (not . Text.isInfixOf "legal fees" . itemBought . fst) pricedPurchases
  let purchasesBeforeDateSix :: Stream (Of (Purchase, Int)) m ()
      purchasesBeforeDateSix = S.takeWhile ((<= 6) . date . fst) $ nonLegalPurchases
  sumPerPerson <- S.fold_ (\acc (purch,pric) -> Map.insertWith (+) (Text.strip . person $ purch) pric acc) Map.empty id purchasesBeforeDateSix
  let values = map snd . Map.toList $ sumPerPerson
  let averageOverAllPersons = (fromIntegral . sum $ values) / (fromIntegral . length $ values)
  pure (sumPerPerson, averageOverAllPersons)

readCSVs :: IO (Stream (Of Price) IO (Maybe SiphonError), Stream (Of Purchase) IO (Maybe SiphonError))
readCSVs = do
  pricesData <- readCsv "../prices.csv"
  purchaseData <- readCsv "../purchases.csv"
  let prices = Siphon.decodeCsvUtf8 siphonPrice pricesData
  let purchases = Siphon.decodeCsvUtf8 siphonPurchase purchaseData
  pure (prices, purchases)
  
data Price = Price
  { item  :: Text
  , price :: Int
  }

decodeInt :: ByteString -> Maybe Int
decodeInt b = do
  (int, leftovers) <- BC8.readInt b
  if leftovers == ""
    then Just int
    else Nothing

decodeText :: ByteString -> Maybe Text
decodeText = Just . TE.decodeUtf8

siphonPrice :: Siphon Headed ByteString Price
siphonPrice = Price
  <$> Siphon.headed "item" decodeText
  <*> Siphon.headed "price" decodeInt

-- colPrice :: Colonnade Headed Price ByteString
-- colPrice = mconcat
--  [ headed "item" (TE.encodeUtf8 . item)
--  , headed "price" (intToBC8 . price)
--  ]

data Purchase = Purchase
  { date        :: Int
  , person      :: Text
  , itemBought  :: Text
  , unitsBought :: Int
  }

siphonPurchase :: Siphon Headed ByteString Purchase
siphonPurchase = Purchase
  <$> Siphon.headed "date" decodeInt
  <*> Siphon.headed "person" decodeText
  <*> Siphon.headed "itemBought" decodeText
  <*> Siphon.headed "unitsBought" decodeInt

--colPurchase :: Colonnade Headed Purchase ByteString
--colPurchase = mconcat
--  [ headed "date" (intToBC8 . date)
--  , headed "person" (TE.encodeUtf8 . person)
--  , headed "itemBought" (TE.encodeUtf8 . itemBought)
--  , headed "unitsBought" (intToBC8 . unitsBought)
--  ]

readCsv :: FilePath -> IO (Stream (Of ByteString) IO ())
readCsv csv = S.readFile csv (pure . id . S.map BC8.pack)
