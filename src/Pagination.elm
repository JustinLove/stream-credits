module Pagination exposing (Paginated(..), paginated)

{-|
# Pagination
@docs Paginated, paginated
-}

import Json.Decode exposing (..)

{-| Record which contains the query total, and a slot to save offset position.

    Kraken.Paginated offset total data
-}
type Paginated a = Paginated Int Int a

{-| Json Decoder for extracting the query total along with the data

    decoder = Kraken.paginated offset Kraken.subscriptions
-}
paginated : Int -> Decoder a -> Decoder (Paginated a)
paginated offset decoder =
  map2 (Paginated offset)
    (field "_total" int)
    decoder
