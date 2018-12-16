-- solution from user gagandeepb, posted to Reddit here:
-- https://www.reddit.com/r/haskell/comments/a50xpr/datahaskell_solve_this_small_problem_to_fill_some/ebnbg1u/
-- and on Github here:
-- https://github.com/gagandeepb/frames-explore/blob/master/src/Lib.hs

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Lib where

import qualified Control.Foldl         as L
import qualified Data.Foldable         as F
import qualified Data.List             as DL
import           Data.Vinyl
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel
import           Frames
import           Frames.ExtraInstances
import           Lens.Micro.Extras
import           Pipes                 hiding (Proxy)
import qualified Pipes.Prelude         as P


-- | = Types
tableTypes "Prices"    "data/prices.csv"
tableTypes "Purchases" "data/purchases.csv"
type Merged = Record ( RecordColumns Prices
                       ++ RDelete Item (RecordColumns Purchases) )
type MoneySpent = "money-spent" :-> Int


-- | = Read data
pricesStream :: MonadSafe m => Producer Prices m ()
pricesStream = readTableOpt pricesParser "data/prices.csv"

loadPrices :: IO (Frame Prices)
loadPrices = inCoreAoS pricesStream

purchasesStream :: MonadSafe m => Producer Purchases m ()
purchasesStream = readTableOpt purchasesParser "data/purchases.csv"

loadPurchases :: IO (Frame Purchases)
loadPurchases = inCoreAoS purchasesStream


-- | = Process

-- | Delete any purchase of legal fees.
loadFilteredPurchase :: IO (Frame Purchases)
loadFilteredPurchase = inCoreAoS $ purchasesStream >-> x where
  x :: Pipe Purchases Purchases (SafeT IO) r
  x = P.filter f
    where  f :: Purchases -> Bool
           f p = rget @Item p /= Field "legal fees (1 hour)"

-- Merge price and purchase data.
joinPricePurchase :: IO (Frame Merged)
joinPricePurchase = innerJoin @'[Item] <$> loadPrices <*> loadFilteredPurchase

zeroSpentColumn :: Int -> [Record '[MoneySpent]]
zeroSpentColumn nrows = replicate nrows $ 0 &: RNil

-- Compute a new column, "money-spent" = units-bought price.
addNewColumn = do
  joined <- joinPricePurchase
  let nrows = F.length joined
      zipped = zipFrames joined $ toFrame $ zeroSpentColumn nrows
      f r = rput field r where
        field = Field @"money-spent"
                $ mult (rget @UnitsBought r) (rget @Price r)
  return $ fmap f zipped

mult :: Num t => ElField '(s1, t) -> ElField '(s2, t) -> t
mult (Field x) (Field y) = (x*y)

-- Group by person.
grouped = do
  a <- addNewColumn
  let persons = F.toList $ view person <$> a
  let uniquePersons = DL.nub persons
  return $
    map (\up ->
          (
            up
          , filterFrame (\r -> (rget @Person r) == Field @"person" up) a
          )
        ) uniquePersons

printGroup = do
  g <- grouped
  mapM_ (\(a, r) -> do print a;  (mapM_ print  r) ) g

-- Within each group: Sort by date in increasing order.
sortedGroups = do
  gs <- grouped
  let gs' = map (\(a, rs) ->
                    (
                      a
                    , DL.sortOn (\r -> unField $ rget @Date r) (F.toList rs)
                    )
                ) gs
  return gs'

unField :: ElField '(s, t) -> t
unField (Field x) = x

type AccumulatedSpending = "accumulated-spending" :-> Int

extractMoneySpent rs = map (\r -> unField (rget @MoneySpent r)) rs

createColumnAccumulated rs =
  toFrame (
              map (\r ->
                      Field @"accumulated-spending" r :& RNil)
                  (
                    DL.scanl1  -- this might be lazy like foldl; can be improved
                      (+)
                      (extractMoneySpent rs)
                  )
          )

-- Compute a new column, "accumulated-spending" = running total of money spent.
addNewColumnInGroups = do
  sorteds <- sortedGroups
  let zipped' = map (\(a, rs) ->
                        (
                          a
                        , zipFrames (toFrame rs) (createColumnAccumulated rs)
                        )
                    ) sorteds
  return zipped'

-- Keep the last row with a date no greater than 6; drop all others.
dropAccordingToDate = do
  ns <- addNewColumnInGroups
  return $
    map (\(a, fr) ->
            (
              a
            , last . F.toList $
                filterFrame (\r ->
                              unField (rget @Date r) <= 6)
                            fr
            )
        )
      ns

-- Across groups, compute the mean of accumulated spending.
meanAcrossGroups = do
  dropped <- dropAccordingToDate
  let temp = map (\(_, r) ->
                    (fromIntegral . unField) $ rget @AccumulatedSpending r)
                  dropped
  let average = (/) <$> L.sum <*> L.genericLength
  return $ L.fold average temp
