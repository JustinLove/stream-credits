module StreamCredits exposing (..)

import View exposing (Host, Follow)


import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Http
import Task
import Time
import Twitch.Helix as Helix
import Twitch.Helix.Decode as Helix
import Twitch.Tmi.Decode as Tmi
import TwitchId
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser
import Url.Parser.Query

type Msg
  = UI (View.Msg)
  | CurrentUrl Url
  | Navigate Browser.UrlRequest
  | WindowSize (Int, Int)
  | FrameStep Float
  | Hosts (Result Http.Error (List Tmi.Host))
  | User (Result Http.Error (List Helix.User))
  | Follows (Result Http.Error (List Helix.Follow))
  | CurrentStream (Result Http.Error (List Helix.Stream))

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , windowWidth : Int
  , windowHeight : Int
  , timeElapsed : Float
  , login : Maybe String
  , userId : Maybe String
  , hosts : List Host
  , follows : List Follow
  , currentFollows : List Follow
  , streamStart : Int
  }

main = Browser.application
  { init = init
  , view = View.document UI
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = Navigate
  , onUrlChange = CurrentUrl
  }

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags location key =
  let
    mlogin = extractSearchArgument "login" location
    muserId = extractSearchArgument "userId" location
  in
  ( { location = location
    , navigationKey = key
    , windowWidth = 480
    , windowHeight = 480
    , timeElapsed = 0
    , login = mlogin
    , userId = muserId
    , hosts = []
    , follows = []
    , currentFollows = []
    , streamStart = 0
    }
  , Cmd.batch 
    [ ( case (muserId, mlogin) of
        (Just id, _) ->
          Cmd.batch
            [ fetchHosts id
            , fetchFollows id
            , fetchStreamById id
            ]
        (Nothing, Just login) -> fetchUserByName login
        (Nothing, Nothing) -> Cmd.none
      )
    , Dom.getViewport
      |> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
      |> Task.perform WindowSize
    ]
  )

update msg model =
  case msg of
    UI (View.SetUsername username) ->
      ( { model | timeElapsed = 0 }, fetchUserByName username )
    CurrentUrl location ->
      ( { model | location = location }, Cmd.none)
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
    WindowSize (width, height) ->
      ( {model | windowWidth = width, windowHeight = height}, Cmd.none)
    FrameStep delta ->
      if View.creditsOff model then
        ( {model | timeElapsed = delta }, Cmd.none )
      else
        ( {model | timeElapsed = model.timeElapsed + delta }, Cmd.none )
    Navigate (Browser.External url) ->
      (model, Navigation.load url)
    User (Ok (user::_)) ->
      let
        m2 =
          { model
          | login = Just user.login
          , userId = Just user.id
          }
      in
      ( m2
      , if (Just user.id) /= model.userId then
          Cmd.batch
            [ Navigation.pushUrl m2.navigationKey (createPath m2)
            , fetchHosts user.id
            , fetchFollows user.id
            , fetchStreamById user.id
            ]
        else if (Just user.login) /= model.login then
          Cmd.batch
            [ Navigation.pushUrl m2.navigationKey (createPath m2)
            ]
        else
          Cmd.none
      )
    User (Ok _) ->
      let _ = Debug.log "user did not find that login name" "" in
      (model, Cmd.none)
    User (Err error) ->
      let _ = Debug.log "user fetch error" error in
      (model, Cmd.none)
    Hosts (Ok twitchHosts) ->
      ( { model
        | hosts = List.map myHost twitchHosts
        }
      , Cmd.none
      )
    Hosts (Err error) ->
      let _ = Debug.log "hosts fetch error" error in
      (model, Cmd.none)
    Follows (Ok twitchFollows) ->
      let
        follows = List.map myFollow twitchFollows
      in
      ( { model
        | follows = follows
        , currentFollows = List.filter (\follow -> follow.followedAt > model.streamStart) follows
        }
      , Cmd.none
      )
    Follows (Err error) ->
      let _ = Debug.log "follows fetch error" error in
      (model, Cmd.none)
    CurrentStream (Ok (stream::_)) ->
      let
        start = (Time.posixToMillis stream.startedAt)
      in
      ( { model
        | streamStart = start
        , currentFollows = List.filter (\follow -> follow.followedAt > start) model.follows
        }
      , Cmd.none
      )
    CurrentStream (Ok _) ->
      let _ = Debug.log "no stream, not live?" "" in
      ( { model
        | streamStart = 0
        }
      , Cmd.none
      )
    CurrentStream (Err error) ->
      let _ = Debug.log "stream fetch error" error in
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize (\w h -> WindowSize (w, h))
    , Browser.Events.onAnimationFrameDelta FrameStep
    ]

myHost : Tmi.Host -> Host
myHost host =
  { hostId = host.hostId
  , hostDisplayName = host.hostDisplayName
  }

fetchHostsUrl : String -> String
fetchHostsUrl id =
  "https://p3szakkejk.execute-api.us-east-1.amazonaws.com/production/hosts?include_logins=1&target=" ++ id

fetchHosts : String -> Cmd Msg
fetchHosts id =
  Http.request
    { method = "GET"
    , headers = []
    , url = fetchHostsUrl id
    , body = Http.emptyBody
    , expect = Http.expectJson Hosts Tmi.hosts
    , timeout = Nothing
    , tracker = Nothing
    }

myFollow : Helix.Follow -> Follow
myFollow follow =
  { fromName = follow.fromName
  , followedAt = Time.posixToMillis follow.followedAt
  }

fetchFollowsUrl : String -> String
fetchFollowsUrl id =
  "https://api.twitch.tv/helix/users/follows?to_id=" ++ id

fetchFollows : String -> Cmd Msg
fetchFollows id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.follows
    , tagger = Follows
    , url = (fetchFollowsUrl id)
    }

fetchUserByNameUrl : String -> String
fetchUserByNameUrl login =
  "https://api.twitch.tv/helix/users?login=" ++ login

fetchUserByName : String -> Cmd Msg
fetchUserByName login =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.users
    , tagger = User
    , url = (fetchUserByNameUrl login)
    }

fetchUserByIdUrl : String -> String
fetchUserByIdUrl id =
  "https://api.twitch.tv/helix/users?id=" ++ id

fetchUserById : String -> Cmd Msg
fetchUserById id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.users
    , tagger = User
    , url = (fetchUserByIdUrl id)
    }

fetchStreamByIdUrl : String -> String
fetchStreamByIdUrl id =
  "https://api.twitch.tv/helix/streams?user_id=" ++ id

fetchStreamById : String -> Cmd Msg
fetchStreamById id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.streams
    , tagger = CurrentStream
    , url = (fetchStreamByIdUrl id)
    }

extractSearchArgument : String -> Url -> Maybe String
extractSearchArgument key location =
  { location | path = "" }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing

createQueryString : Model -> List Url.QueryParameter
createQueryString model =
  [ Maybe.map (Url.string "userId") model.userId
  , Maybe.map (Url.string "login") model.login
  ]
    |> List.filterMap identity

createPath : Model -> String
createPath model =
  Url.relative [] (createQueryString model)
