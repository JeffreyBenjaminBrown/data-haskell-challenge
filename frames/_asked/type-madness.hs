*Lib Lib> :i joinPricePurchase
joinPricePurchase ::
  IO
    (Frame
       (Record
          '[ '("item", Text), '("price", Int), '("date", Int),
            '("person", Text), UnitsBought]))

*Lib Lib> :i Purchases 
type Purchases = Record '[Date, Person, Item, UnitsBought]

*Lib Lib> :i Item Price Date Person UnitsBought 
type Item =
  "item" :-> Text :: (ghc-prim-0.5.2.0:GHC.Types.Symbol, *)
type Price =
  "price" :-> Int :: (ghc-prim-0.5.2.0:GHC.Types.Symbol, *)
type Date =
  "date" :-> Int :: (ghc-prim-0.5.2.0:GHC.Types.Symbol, *)
type Person =
  "person" :-> Text :: (ghc-prim-0.5.2.0:GHC.Types.Symbol, *)
type UnitsBought =
  "units-bought" :-> Int :: (ghc-prim-0.5.2.0:GHC.Types.Symbol, *)

*Lib Lib> 
