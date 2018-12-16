-- solution from user gagandeepb, posted to Reddit here:
-- https://www.reddit.com/r/haskell/comments/a50xpr/datahaskell_solve_this_small_problem_to_fill_some/ebnbg1u/
-- and on Github here:
-- https://github.com/gagandeepb/frames-explore/blob/master/src/Lib.hs

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
type MergedAndSpent = Record ( RecordColumns Merged ++ '[MoneySpent] )

type AccumulatedSpending = "accumulated-spending" :-> Int
type MergedAndAccum = Record ( RecordColumns MergedAndSpent
                               ++ '[AccumulatedSpending] )

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
dropLegalCostsIO :: IO (Frame Purchases)
dropLegalCostsIO = inCoreAoS $ purchasesStream >-> dropLegalCosts

dropLegalCosts :: Pipe Purchases Purchases (SafeT IO) r
dropLegalCosts = P.filter f
  where  f :: Purchases -> Bool
         f p = rget @Item p /= Field "legal fees (1 hour)"

-- Merge price and purchase data.
joinPricePurchaseIO :: IO (Frame Merged)
joinPricePurchaseIO = do pr <- inCoreAoS pricesStream
                         pu <- inCoreAoS $ purchasesStream >-> dropLegalCosts
                         return $ joinPricePurchase pr pu

joinPricePurchase :: Frame Prices -> Frame Purchases -> Frame Merged
joinPricePurchase pr pu = innerJoin @'[Item] pr pu

emptySpendingColumn :: Int -> [Record '[MoneySpent]]
emptySpendingColumn nrows = replicate nrows $ 0 &: RNil

-- Compute a new column, "money-spent" = units-bought price.
addSpendingIO :: IO (Frame MergedAndSpent)
addSpendingIO = addSpending <$> joinPricePurchaseIO

addSpending :: Frame Merged -> Frame MergedAndSpent
addSpending fr = fmap f zipped where
  nrows = F.length fr :: Int
  zipped :: Frame MergedAndSpent
  zipped = zipFrames fr $ toFrame $ emptySpendingColumn nrows
  f :: MergedAndSpent -> MergedAndSpent
  f r = rput field r where
    mult :: Num t => ElField '(s1, t) -> ElField '(s2, t) -> t
    mult (Field x) (Field y) = (x*y)
    field = Field @"money-spent" $ mult (rget @UnitsBought r) (rget @Price r)

-- Group by person.
groupedByPersonIO :: IO [(Text, Frame MergedAndSpent)]
groupedByPersonIO = groupedByPerson <$> addSpendingIO

groupedByPerson :: Frame MergedAndSpent -> [(Text, Frame MergedAndSpent)]
groupedByPerson fr = map f uniquePersons where
  persons :: [Text] -- TODO This should be Person, not Text
  persons = F.toList $ view person <$> fr
  uniquePersons = DL.nub persons
  f up = ( up
         , filterFrame (\r -> (rget @Person) r == Field @"person" up) fr )
-- TODO ? WHy is this not equivalent?
--         , filterFrame ((==) Field @"person" up . (rget @Person)) fr )

printGroups = do
  g <- groupedByPersonIO
  mapM_ (\(a, r) -> do print a;  (mapM_ print  r); putStrLn "" ) g

-- Within each group: Sort by date in increasing order.
sortGroupsByDateIO :: IO [( Text, [MergedAndSpent] )]
sortGroupsByDateIO = map sortGroupsByDate <$> groupedByPersonIO

sortGroupsByDate :: ( Text, Frame MergedAndSpent )
                 -> ( Text, [MergedAndSpent]     )
sortGroupsByDate (a, rs) = ( a, DL.sortOn f $ F.toList rs)
  where f r = unField $ rget @Date r

unField :: ElField '(s, t) -> t
unField (Field x) = x

extractMoneySpent rs = map (\r -> unField (rget @MoneySpent r)) rs

createColumnAccumulated rs = toFrame
  ( map
    (\r -> Field @"accumulated-spending" r :& RNil)
    ( DL.scanl1  -- this might be lazy like foldl; can be improved
      (+)
      (extractMoneySpent rs)
    )
  )

-- Compute a new column, "accumulated-spending" = running total of money spent.
addNewColumnInGroupsIO :: IO [(Text, Frame MergedAndAccum)]
addNewColumnInGroupsIO = map addNewColumnInGroups <$> sortGroupsByDateIO

addNewColumnInGroups :: (Text, [MergedAndSpent])
                     -> (Text, Frame MergedAndAccum)
addNewColumnInGroups (a, rs) = (a, rs')
  where rs' = zipFrames (toFrame rs) (createColumnAccumulated rs)

-- Keep the last row with a date no greater than 6; drop all others.
dropAccordingToDate = do
  ns <- addNewColumnInGroupsIO
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
