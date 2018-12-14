module View exposing (Msg(..), Host, document, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events

type Msg
  = None

type alias Host =
  { hostId : String
  , hostDisplayName : String
  }


css = """
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
"""

document tagger model =
  { title = "Stream Credits"
  , body = [Html.map tagger (view model)]
  }

view model = 
  div [ class "view" ]
    [ node "style" [] [ text css ]
    , text "view"
    ]
