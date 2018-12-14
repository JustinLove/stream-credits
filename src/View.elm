module View exposing (Msg(..), document, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events

type Msg
  = None

css = """
"""

document tagger model =
  { title = "StreamCredits"
  , body = [Html.map tagger (view model)]
  }

view model = 
  div [ class "view" ]
    [ node "style" [] [ text css ]
    , text "view"
    ]
