module View exposing (Msg, Host, Raid, Cheer, Sub, Follow, document, view, creditsOff)

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

type alias Msg
  = Never

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
  , points : Int
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
              [ [""]
                |> displaySection model.windowWidth model.windowHeight "Thanks for watching!"
              , el [ centerX ] <|
                displayLogin model
              ]
          else
            column
              [ width fill
              , spacing (sectionSpacing model.windowHeight)
              , moveDown ((toFloat model.windowHeight) + leadingGap - (model.timeElapsed * (scrollSpeed model.windowHeight)))
              , htmlAttribute <| Html.Attributes.class "credits"
              ]
              (applySections (displaySection model.windowWidth model.windowHeight) model)
        ]
    ]

applySections fun model =
  [ [""]
    |> fun "Thanks for watching!"
  , model.cheers
    |> Dict.values
    |> List.sortBy (.amount>>negate)
    |> List.map .displayName
    |> fun "Thanks for the Bits!"
  , model.bitsLeaders
    |> List.sortBy (.amount>>negate)
    |> List.map .displayName
    |> fun "Weekly Bits Leaders"
  , model.subs
    |> Dict.values
    |> List.sortWith subRank
    |> List.map .displayName
    |> fun "Thanks for the Subs!"
  , model.raids
    |> List.sortBy (.viewerCount>>negate)
    |> List.map .displayName
    |> fun "Thanks for Raiding!"
  , model.hosts
    |> notRaids model.raids
    |> List.map .hostDisplayName
    |> List.sort
    |> fun "Thanks for Hosting!"
  , model.currentFollows
    |> List.map .fromName
    |> fun "Thanks for Following!"
  , model.subscribers
    |> List.sortWith subRank
    |> List.filter (\sub -> (Just sub.userId) /= model.userId)
    |> List.map .displayName
    |> fun "Subscribers"
  ]

subRank : Sub -> Sub -> Order
subRank a b =
  if b.points < a.points then
    LT
  else if b.points > a.points then
    GT
  else if b.months < a.months then
    LT
  else if b.months > a.months then
    GT
  else
    compare a.displayName b.displayName

notRaids : List Raid -> List Host -> List Host
notRaids raids = 
  List.filter (\host ->
    raids
      |> List.filter (\raid -> raid.userId == host.hostId)
      |> List.isEmpty
  )

displaySection : Int -> Int -> String -> List String -> Element Msg
displaySection windowWidth windowHeight title items =
  if List.isEmpty items then
    none
  else
    let
      (_, columns) = rowsAndColumns windowWidth windowHeight items
    in
    column [ spacing (columnSpacing windowHeight), width fill]
      [ el
        [ Region.heading 1
        , Font.size (headingFontSize windowHeight)
        , Font.bold
        , centerX
        ]
        ( text title )
      , row
        [ centerX
        , spacing (headingSpacing windowHeight)
        ]
        (items
          |> List.map displayItem
          |> splitList columns
          |> List.map (column [ centerX, alignTop, spacing (creditSpacing windowHeight)])
        )
      ]

measureSection : Int -> Int -> String -> List String -> Int
measureSection windowWidth windowHeight _ items =
  let
    (rows, _) = rowsAndColumns windowWidth windowHeight items
  in
  maximumSectionSize windowHeight rows

maximumSectionSize : Int -> Int -> Int
maximumSectionSize height count =
  headingFontSize height
  + headingSpacing height
  + count * creditFontSize height
  + (count - 1) * creditSpacing height
  + sectionSpacing height

rowsAndColumns : Int -> Int -> List String -> (Int, Int)
rowsAndColumns windowWidth windowHeight items =
  let
    count = List.length items
    sectionHeight = maximumSectionSize windowHeight count
    widest = items
      |> List.map String.length
      |> List.maximum
      |> Maybe.withDefault 1
    baseWidest = widest // 2 * (creditFontSize windowHeight) + (columnSpacing windowHeight)
    columns = min ((windowWidth - (margins windowHeight))//baseWidest) (sectionHeight//windowHeight)
    rows = count // columns
  in
    (rows, columns)

splitList : Int -> List a -> List (List a)
splitList parts list =
  if parts < 2 then
    [list]
  else
    let
      chop = ((List.length list)//parts)
    in
      (List.take chop list) :: (splitList (parts-1) (List.drop chop list))

displayItem : String -> Element Msg
displayItem item =
  el
    [ Region.heading 3
    , Font.bold
    ]
    (text item)

displayLogin model =
  case model.auth of
    Just _ ->
      row [ spacing (scaled model.windowHeight -2) ]
        [ text <| Maybe.withDefault "--" model.authLogin
        , link []
            { url = (Url.relative [] [])
            , label = text "logout"
            }
        ]
    Nothing ->
      link []
        { url = authorizeUrl (urlForRedirect model.location)
        , label = row [] [ icon "twitch", text "login" ]
        }

authorizeUrl : String -> String
authorizeUrl redirectUri =
  "https://api.twitch.tv/kraken/oauth2/authorize"
    ++ (
      [ Url.string "client_id" TwitchId.clientId
      , Url.string "redirect_uri" redirectUri
      , Url.string "response_type" "token"
      , Url.string "scope" "chat:read bits:read channel:read:subscriptions"
      ]
      |> Url.toQuery
      )

urlForRedirect : Url -> String
urlForRedirect url =
  {url | query = Nothing, fragment = Nothing } |> Url.toString

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
    , case model.auth of
      Just _ ->
        displayLogin model
      Nothing ->
        none
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
  + (applySections (measureSection model.windowWidth model.windowHeight) model
      |> List.foldl (+) 0
    )
  - (sectionSpacing model.windowHeight) -- added spacing to every section, even the last
  + trailingGap

scrollSpeed : Int -> Float
scrollSpeed height = ((toFloat height)/10)/1000

creditFontSize height = scaled height 1
creditSpacing height = scaled height -5
headingFontSize height = scaled height 4
headingSpacing height = scaled height 1
sectionSpacing height = scaled height 6
columnSpacing height = scaled height 1
margins height = scaled height 12
leadingGap = 60
trailingGap = 60

scaled height = modular (max ((toFloat height)/30) 15) 1.25 >> round
