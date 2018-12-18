module View exposing (Msg(..), Host, document, view, creditsOff)

import Element exposing (..)
import Element.Background as Background
import Element.Region as Region
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (on)
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
      , Font.size (creditFontSize model.windowHeight)
      , Font.family
        [ Font.typeface "Times New Roman"
        ]
      ] <|
      column [ height fill, width fill, clip, behindContent displayFooter ]
        [ if List.isEmpty model.hosts then
            column
              [ width fill
              , spacing (headingSpacing model.windowHeight)
              , centerY
              ]
              [ el
                [ Region.heading 1
                , Font.size (headingFontSize model.windowHeight)
                , Font.bold
                , centerX
                ]
                (text "Thanks for watching!")
              , el [ centerX ] <|
                case model.login of
                  Just name -> text ""
                  Nothing ->
                    case model.userId of
                      Just _ -> text ""
                      Nothing -> displayNameEntryBox model.login
              ]
          else
            column
              [ width fill
              , spacing (headingSpacing model.windowHeight)
              , moveDown ((toFloat model.windowHeight) + leadingGap - (model.timeElapsed * (scrollSpeed model.windowHeight)))
              ]
              [ el
                [ Region.heading 1
                , Font.size (headingFontSize model.windowHeight)
                , Font.bold
                , centerX
                ]
                ( text "Thanks for hosting!" )
              , model.hosts
                |> List.map displayHost
                |> column [ centerX, spacing (creditSpacing model.windowHeight) ]
              ]
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
  el [] <|
  html <|
    Html.div [ Html.Attributes.class "name-entry" ]
      [ Html.label [ Html.Attributes.for "channelname" ] [ Html.text "Channel Name" ]
      , Html.text " "
      , Html.input
        [ Html.Attributes.type_ "text"
        , Html.Attributes.id "channelname"
        , Html.Attributes.name "channelname"
        , Html.Attributes.placeholder (Maybe.withDefault "" login)
        , on "change" <| targetValue Json.Decode.string SetUsername
        ] []
      ]

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

displayFooter : Element msg
displayFooter =
  row [ Region.footer, spacing 10, alignBottom, Font.size (scaled 500 -2) ]
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

--creditsOff : Model -> Bool
creditsOff model =
  round (model.timeElapsed * (scrollSpeed model.windowHeight)) > (model.windowHeight + creditSize model)

--creditSize : Model -> Int
creditSize model =
  (List.length model.hosts) * creditFontSize model.windowHeight
  + ((List.length model.hosts) - 1) * creditSpacing model.windowHeight
  + headingFontSize model.windowHeight
  + headingSpacing model.windowHeight
  + leadingGap + trailingGap

scrollSpeed : Int -> Float
scrollSpeed height = ((toFloat height)/10)/1000

creditFontSize height = scaled height 1
creditSpacing height = scaled height -5
headingFontSize height = scaled height 4
headingSpacing height = scaled height 1
leadingGap = 60
trailingGap = 60

scaled height = modular (max ((toFloat height)/30) 15) 1.25 >> round
