shorten and clarify the type name of a join when using Vinyl and/or Frames
--
(This might be a purely Vinyl question, or purely Frames; I can't tell.)

I am studying [some code](https://github.com/JeffreyBenjaminBrown/data-haskell-challenge/blob/master/frames/src/Lib.hs) by /u/gagandeepb that defines two kinds of records:

```
*Lib Lib> :i Purchases 
type Purchases = Record '[Date, Person, Item, UnitsBought]

> :i Prices
type Prices = Record '[Item, Price]
```

and a function that merges them:

```
*Lib Lib> :i joinPricePurchase
joinPricePurchase :: IO (Frame (Record '[ '("item", Text)
                                        , '("price", Int)
                                        , '("date", Int)
                                        , '("person", Text)
                                        , UnitsBought]))
```

(Although the `UnitsBought` field shows up differently there for some reason, it's the same as the others:

```
*Lib Lib> :i Item Price Date Person UnitsBought 
type Item =
  "item"         :-> Text :: (ghc-prim-0.5.2.0:GHC.Types.Symbol, *)
type Price =
  "price"        :-> Int  :: (ghc-prim-0.5.2.0:GHC.Types.Symbol, *)
type Date =
  "date"         :-> Int  :: (ghc-prim-0.5.2.0:GHC.Types.Symbol, *)
type Person =
  "person"       :-> Text :: (ghc-prim-0.5.2.0:GHC.Types.Symbol, *)
type UnitsBought =
  "units-bought" :-> Int  :: (ghc-prim-0.5.2.0:GHC.Types.Symbol, *)
```

I'd like to find or define some kind of operator that would allow the type signature of `joinPricePurchase` to make clear the relationship bewteen its output type and the `Purchases` and `Prices` types. Something like

```
joinPricePurchase :: IO (Frame (Merge Prices Purchases))
```

or

```
joinPricePurchase :: IO (Frame (Record (RecordColumns Purchases ++: RecordColumns Prices)))
```

Is that possible?

(One potentially bothersome detail is that the order of the fields in the output of `joinPricePurchase` cannot be gotten by simply concatenating the fields in the two input data types. If you did that you would have two `Item` fields.)
