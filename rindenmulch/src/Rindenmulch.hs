-- Solution submitted by Reddit user rindenmulch.
-- https://www.reddit.com/r/haskell/comments/a50xpr/datahaskell_solve_this_small_problem_to_fill_some/ebkx2lh/

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Rindenmulch where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           GHC.Generics (Generic)


type Money = Double
type Person = Text
type Date = Int
type Item = Text

data Price = Price {
  item :: Item
, price :: Money
} deriving (Generic, Show)

data Purchase = Purchase {
  date :: Date
, person :: Person
, itemBought :: Item
, unitsBought :: Int
} deriving (Generic, Show)

instance FromRecord Price
instance FromRecord Purchase


solve :: BL.ByteString -> BL.ByteString -> Either String (Map Person Money, Money)
solve pricesData purchaseData = do
  (prices    :: Vector Price)    <- decode HasHeader pricesData
  (purchases :: Vector Purchase) <- decode HasHeader purchaseData

  let
    (priceMapping :: Map Item Money) = M.fromList . map f . V.toList $ prices
      where f (Price i p) = (T.strip i, p)

    mapWithPrice :: Purchase -> Maybe (Purchase, Money)
    mapWithPrice purchase =
      f <$> M.lookup (T.strip $ itemBought purchase) priceMapping
      where f price = (purchase, price * fromIntegral (unitsBought purchase))

    pricedPurchases, nonLegalPurchases, purchasesBeforeDate6 :: Vector (Purchase, Money)
    pricedPurchases = V.mapMaybe mapWithPrice purchases
    nonLegalPurchases = V.filter f pricedPurchases
      where f = not . T.isInfixOf "legal fees" . itemBought . fst
    purchasesBeforeDate6 = V.takeWhile f . V.reverse $ nonLegalPurchases
      where f = (<= 6) . date . fst

    -- Accumulate purchases per person
    (sumPerPerson :: Map Person Money) = V.foldr' f M.empty purchasesBeforeDate6 where
      f (purchase, price) acc = M.insertWith (+) (T.strip . person $ purchase) price acc

    -- Calculate average over all persons (who have valid purchases before or at time 6)
    averageOverAllPersons :: Double
    averageOverAllPersons = (sum $ vs) / (fromIntegral . length $ vs)
      where vs = map snd . M.toList $ sumPerPerson

  return (sumPerPerson, averageOverAllPersons)


main :: IO ()
main = do
  -- Read CSV files
  pricesData   <- BL.readFile   "../prices.csv"
  purchaseData <- BL.readFile "../purchases.csv"
  putStrLn $ show $ solve pricesData purchaseData
