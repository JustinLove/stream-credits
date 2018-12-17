module View exposing (Msg(..), Host, document, view)

import Element exposing (..)
import Element.Background as Background
import Element.Region as Region
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
--import Html.Events exposing (on)
import Json.Decode
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)

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
.view {
  height: 100%;
}
svg.icon {
  display: inline-block;
  width: 1em;
  height: 1em;
  vertical-align: -0.2em;
  stroke-width: 0;
  stroke: currentColor;
  fill: currentColor;
}
.icon-github { color: #888; }
.icon-twitter { color: #55acee; }
.icon-twitch { color: #6441A4; }
a:link, a:visited { color: #b19dd8; }
a:hover, a:active { color: rgb(218, 216, 222); }
"""

document tagger model =
  { title = "Stream Credits"
  , body = [Html.map tagger (view model)]
  }

view model = 
  Html.div [ Html.Attributes.class "view" ]
    [ Html.node "style" [] [ Html.text css ]
    , layout
      [ Background.color (rgb255 23 20 31)
      , height fill
      , Font.color (rgb255 218 216 222)
      , Font.size (scaled 1)
      , Font.family
        [ Font.typeface "Times New Roman"
        ]
      ] <|
      column [ height fill, width fill ]
        [ if List.isEmpty model.hosts then
            column []
              [ el [ Region.heading 1 ] (text "Thanks for watching!")
              , case model.login of
                Just name -> el [ Region.heading 2 ] (text ("/host " ++ name))
                Nothing ->
                  case model.userId of
                    Just _ -> text ""
                    Nothing -> displayNameEntryBox model.login
              ]
          else
            column [ width fill, spacing 20]
              [ el
                [ Region.heading 1
                , Font.size (scaled 4)
                , Font.bold
                , centerX
                ]
                ( text "Thanks for hosting!" )
              , model.hosts
                |> List.map displayHost
                |> column [ centerX, spacing 8 ]
              ]
        , displayFooter
        ]
    ]

displayHost : Host -> Element Msg
displayHost host =
  el
    [ Region.heading 3
    , Font.bold
    ]
    (text host.hostDisplayName)

displayNameEntryBox : Maybe String -> Element Msg
displayNameEntryBox login =
  Input.username
    []
    { label = Input.labelRight [] (text "Channel Name")
    , placeholder = Maybe.map (text >> (Input.placeholder [])) login
    , onChange = SetUsername
    , text = ""
    }

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

displayFooter : Element msg
displayFooter =
  row [ Region.footer, spacing 10, alignBottom, Font.size (scaled -2) ]
    [ link []
      { url = "https://github.com/JustinLove/stream-credits"
      , label = row [] [ icon "github", text "stream-credits" ]
      }
    , link []
      { url = "https://twitter.com/wondible"
      , label = row [] [ icon "twitter", text "@wondible" ]
      }
    , link []
      { url = "https://twitch.tv/wondible"
      , label = row [] [ icon "twitch", text "wondible" ]
      }
    ]

icon : String -> Element msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]
  |> html

scaled = modular 20 1.25 >> round
