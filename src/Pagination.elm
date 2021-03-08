module Pagination exposing (Paginated(..), paginated)

{-|
# Pagination
@docs Paginated, paginated
-}

import Json.Decode exposing (..)

{-| Record which contains the query total, and a slot to save offset position.

    Helix.Paginated cursor data
-}
type Paginated a = Paginated (Maybe String) a

{-| Json Decoder for extracting the cursor along with the data

    decoder = Helix.paginated offset Helix.subscriptions
-}
paginated : Decoder a -> Decoder (Paginated a)
paginated decoder =
  map2 Paginated
    (field "pagination" (maybe (field "cursor" string)))
    decoder
