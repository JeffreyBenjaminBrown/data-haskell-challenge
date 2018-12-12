import pandas as pd
import numpy as np


prices    = pd.read_csv( "prices.csv" )
purchases = pd.read_csv( "purchases.csv" )

def strip_space_from_column_names_and_text_columns( data ):
  data = data.rename(columns=lambda x: x.strip())
  for c in data.columns:
    if data[c].dtype == 'O': data[c] = data[c].apply( str.strip )
  return data

prices    = strip_space_from_column_names_and_text_columns( prices )
purchases = strip_space_from_column_names_and_text_columns( purchases )
  # why the above line cannot be merged into the next, I don't know.

purchases = purchases[ purchases[ "item-bought" ] != "legal fees (1 hour)"
          ] . merge( prices, left_on="item-bought", right_on="item"
          ) . drop( columns = "item-bought" )

purchases["spent"] = purchases["units-bought"] * purchases["price"]

gs = purchases . groupby( "person" )

def process_group( g ):
  g =   g[ g["date"] <= 6
    ] . sort_values( by="date", ascending=False )
  g[ "spent-accum" ] = g["spent"].cumsum()
  return g . tail(1)

gs.apply(   process_group
        )   [ "spent-accum"
        ] . agg( "mean" )
