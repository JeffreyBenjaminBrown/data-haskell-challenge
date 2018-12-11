-- Posted by Reddit user tomejaguar, who I'm guessing is Tom Ellis, main author of Opaleye.
-- https://www.reddit.com/r/haskell/comments/a50xpr/datahaskell_solve_this_small_problem_to_fill_some/ebjv5gd/

{-# LANGUAGE Arrows #-}

-- Extensions only needed for the generated code.  Will disappear in a future release.
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Lib where

import Opaleye
import Control.Arrow (returnA)
import Data.Profunctor.Product (p2)

-- Imports only needed for the generated code.  Will disappear in a future release.
import Data.Profunctor as P
import Opaleye.TypeFamilies
import Data.Profunctor.Product as PP
import Data.Profunctor.Product.Default as D
import Data.Profunctor.Product.Default (Default)


data Item f = Item {
    iName  :: TableField f String SqlText   NN Req
  , iPrice :: TableField f Double SqlFloat8 NN Req
  }

data Purchase f = Purchase {
    pDate  :: TableField f Int    SqlInt4   NN Req
  , pName  :: TableField f String SqlText   NN Req
  , pItemN :: TableField f String SqlText   NN Req
  , pUnits :: TableField f Double SqlFloat8 NN Req
  }

items :: Table (Item W) (Item O)
items = table "items" (Item <$> lmap iName  (tableColumn "name")
                            <*> lmap iPrice (tableColumn "price"))

purchases :: Table (Purchase W) (Purchase O)
purchases = table "purchases" (Purchase
                            <$> lmap pDate  (tableColumn "date")
                            <*> lmap pName  (tableColumn "name")
                            <*> lmap pItemN (tableColumn "item")
                            <*> lmap pUnits (tableColumn "units"))

query :: Select (Column SqlFloat8)
query = aggregate avg $
        fmap snd $
        aggregate (p2 (groupBy, Opaleye.sum)) $
  proc () -> do
  item     <- selectTable items -< ()
  purchase <- selectTable purchases -< ()
  restrict -< iName item .== pItemN purchase
  restrict -< pDate purchase .<= 6
  restrict -< pItemN purchase ./= sqlString "legal fees (1 hour)"
  returnA -< (pName purchase, pUnits purchase * iPrice item)

sql :: IO ()
sql = case showSql query of
  Nothing -> return ()
  Just s  -> putStrLn s
