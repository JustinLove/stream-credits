module View exposing (Msg(..), Host, document, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode

type Msg
  = SetUsername String

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

    , if List.isEmpty model.hosts then
        div []
          [ h1 [ class "thanks" ] [ text "Thanks for watching!" ]
          , case model.login of
            Just name -> h2 [ class "host-command" ] [ text ("/host " ++ name) ]
            Nothing ->
              case model.userId of
                Just _ -> text ""
                Nothing -> displayNameEntryBox model.login
          ]
      else
        div []
          [ h1 [ class "thanks" ] [ text "Thanks for hosting!" ]
          , model.hosts
            |> List.map displayHost
            |> ul []
          ]
    ]

displayHost : Host -> Html Msg
displayHost host =
  li [] [ h3 [] [ text host.hostDisplayName ] ]

displayNameEntryBox : Maybe String -> Html Msg
displayNameEntryBox login =
  div [ class "name-entry" ]
    [ label [ for "channelname" ] [ text "Channel Name" ]
    , text " "
    , input
      [ type_ "text"
      , id "channelname"
      , name "channelname"
      , placeholder (Maybe.withDefault "" login)
      , on "change" <| targetValue Json.Decode.string SetUsername
      ] []
    ]

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)
