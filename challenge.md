DataHaskell: Solve this small problem to fill a big gap in the documentation.
---


# Why

I've been a data (specifically economics) programmer for maybe a decade. The vast majority of the work occupies, honestly, a small problem space.

I just got the OK to use Haskell instead of Python at work[1]. Looking through [the DataHaskell documentation](http://www.datahaskell.org/docs/), it is not clear to me how to do a few of the bread-and-butter data programming operations.

If you provided code to solve the following small problem, I think you'd be serving a huge fraction of DataHaskell newcomers.


# The problem

Averaged across persons, excluding legal fees, how much money had each person spent by time 6?

```
item                , price
computer            , 1000
car                 , 5000
legal fees (1 hour) , 400

date , person , item-bought         , units-bought
7    , bob    , car                 , 1
5    , alice  , car                 , 1
4    , bob    , legal fees (1 hour) , 20
3    , alice  , computer            , 2
1    , bob    , computer            , 1
```

It would be extra cool if you provided both an in-memory and a streaming solution.


# Principles|operations it illustrates

Predicate-based indexing|filtering.
Merging.
Within- and across-group operations.
Sorting.
Accumulation (what Data.List calls "scanning").
Projection (both the "last row" and the "mean" operations).
Statistics (the "mean" operation).


# Solution and proposed algorithm (it's possible you don't want to read this)

The answer is $4000. That's because by time 6, Bob had bought 1 computer ($1000) and 20 hours of legal work (excluded), while Alice had bought a car ($5000) and two computers ($2000).

One way to compute that would be to:
```
  Delete any purchase of legal fees.
  Merge price and purchase data.
  Compute a new column, "money-spent" = units-bought   price.
  Group by person. Within each group:
    Sort by date in increasing order.
    Compute a new column, "accumulated-spending" = running total of money spent.
    Keep the last row with a date no greater than 6; drop all others.
  Across groups, compute the mean of accumulated spending.
```

# Footnotes

[1] I work for the [Observatorio Fiscal](https://www.ofiscal.org/). We publish, for free and online, analysis of the taxing and spending of the Colombian government. [All our code](https://github.com/JeffreyBenjaminBrown/tax.co) is open source.