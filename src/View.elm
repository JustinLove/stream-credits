module View exposing (Msg(..), Host, Raid, Cheer, Sub, Follow, document, view, creditsOff)

import TwitchId

import Dict
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
import Url exposing (Url)
import Url.Builder as Url

type Msg
  = SetUsername String

type alias Host =
  { hostId : String
  , hostDisplayName : String
  }

type alias Raid =
  { userId : String
  , displayName : String
  , viewerCount : Int
  }

type alias Cheer =
  { userId : String
  , displayName : String
  , amount : Int
  }

type alias Sub =
  { userId : String
  , displayName : String
  , months : Int
  , tier : Int
  }

type alias Follow =
  { fromName : String
  , followedAt : Int
  }


css = """
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
.view {
  height: 100%;
}
.credits { animation: delay-appear 2s; }
@keyframes delay-appear {
  0% { opacity: 0; }
  50% { opacity: 0; }
  100% { opacity: 1; }
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
        [ Font.typeface "Helvetica"
        ]
      ] <|
      column [ height fill, width fill, clip, inFront (displayFooter model) ]
        [ if model.login == Nothing && model.userId == Nothing then
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
                displayNameEntryBox model.login
              ]
          else
            column
              [ width fill
              , spacing (sectionSpacing model.windowHeight)
              , moveDown ((toFloat model.windowHeight) + leadingGap - (model.timeElapsed * (scrollSpeed model.windowHeight)))
              , htmlAttribute <| Html.Attributes.class "credits"
              ]
              [ el
                [ Region.heading 1
                , Font.size (headingFontSize model.windowHeight)
                , Font.bold
                , centerX
                ]
                (text "Thanks for watching!")
              , model.cheers
                |> Dict.values
                |> List.map .displayName
                |> displaySection model.windowHeight "Thanks for the Bits!"
              , model.subs
                |> List.map .displayName
                |> displaySection model.windowHeight "Thanks for the Subs!"
              , model.raids
                |> List.map .displayName
                |> displaySection model.windowHeight "Thanks for Raiding!"
              , model.hosts
                |> notRaids model.raids
                |> List.map .hostDisplayName
                |> displaySection model.windowHeight "Thanks for Hosting!"
              , model.currentFollows
                |> List.map .fromName
                |> displaySection model.windowHeight "Thanks for Following!"
              ]
        ]
    ]

notRaids : List Raid -> List Host -> List Host
notRaids raids = 
  List.filter (\host ->
    raids
      |> List.filter (\raid -> raid.userId == host.hostId)
      |> List.isEmpty
  )

displaySection : Int -> String -> List String -> Element Msg
displaySection height title items =
  if List.isEmpty items then
    none
  else
    column [ spacing (headingSpacing height), width fill]
      [ el
        [ Region.heading 1
        , Font.size (headingFontSize height)
        , Font.bold
        , centerX
        ]
        ( text title )
      , items
        |> List.map displayItem
        |> column [ centerX, spacing (creditSpacing height) ]
      ]

displayItem : String -> Element Msg
displayItem item =
  el
    [ Region.heading 3
    , Font.bold
    ]
    (text item)

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

--displayFooter : Model -> Element msg
displayFooter model =
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
  leadingGap
  + headingFontSize model.windowHeight
  + headingSpacing model.windowHeight
  + (List.length model.hosts) * creditFontSize model.windowHeight
  + ((List.length model.hosts) - 1) * creditSpacing model.windowHeight
  + sectionSpacing model.windowHeight
  + headingFontSize model.windowHeight
  + headingSpacing model.windowHeight
  + (List.length model.currentFollows) * creditFontSize model.windowHeight
  + ((List.length model.currentFollows) - 1) * creditSpacing model.windowHeight
  + trailingGap

scrollSpeed : Int -> Float
scrollSpeed height = ((toFloat height)/10)/1000

creditFontSize height = scaled height 1
creditSpacing height = scaled height -5
headingFontSize height = scaled height 4
headingSpacing height = scaled height 1
sectionSpacing height = scaled height 6
leadingGap = 60
trailingGap = 60

scaled height = modular (max ((toFloat height)/30) 15) 1.25 >> round
