module StreamCredits exposing (..)

import PortSocket
import Twitch.Tmi.Chat as Chat
import View exposing (Host, Follow)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Http
import Json.Decode as Decode
import Parser.Advanced as Parser
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
  | Self (Result Http.Error (List Helix.User))
  | Follows (Result Http.Error (List Helix.Follow))
  | CurrentStream (Result Http.Error (List Helix.Stream))
  | WebSocketTest PortSocket.Id PortSocket.Event

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , windowWidth : Int
  , windowHeight : Int
  , timeElapsed : Float
  , login : Maybe String
  , userId : Maybe String
  , auth : Maybe String
  , authLogin : Maybe String
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
    mauth = extractHashArgument "access_token" location
  in
  ( { location = location
    , navigationKey = key
    , windowWidth = 480
    , windowHeight = 480
    , timeElapsed = 0
    , login = mlogin
    , userId = muserId
    , auth = mauth
    , authLogin = Nothing
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
    , fetchSelf mauth
    , Dom.getViewport
      |> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
      |> Task.perform WindowSize
    , PortSocket.connect "wss://irc-ws.chat.twitch.tv:443"
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
    Self (Ok (user::_)) ->
      let
        m2 =
          { model
          | authLogin = Just user.login
          }
      in
      ( m2
      , Cmd.none
      )
    Self (Ok _) ->
      let _ = Debug.log "self lookup did not find a user" "" in
      (model, Cmd.none)
    Self (Err error) ->
      let _ = Debug.log "self fetch error" error in
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
    WebSocketTest id (PortSocket.Error value) ->
      let _ = Debug.log "websocket error" value in
      (model, Cmd.none)
    WebSocketTest id PortSocket.Open ->
      let _ = Debug.log "websocket open" id in
      Maybe.map2 (\auth login -> 
        (model, Cmd.batch
          -- order is reversed, because elm feels like it
          [ PortSocket.send id ("NICK " ++ login)
          , PortSocket.send id ("PASS oauth:" ++ auth)
          ])
        )
        model.auth
        model.authLogin
      |> Maybe.withDefault
        (model, PortSocket.send id ("NICK justinfan1234"))
    WebSocketTest id PortSocket.Close ->
      let _ = Debug.log "websocket closed" id in
      (model, Cmd.none)
    WebSocketTest id (PortSocket.Message message) ->
      let _ = Debug.log "websocket message" message in
      case (Parser.run Chat.message message) of
        Ok lines ->
          List.foldl (reduce (chatResponse id)) (model, Cmd.none) lines
        Err err ->
          let _ = Debug.log "message parse failed" err in
          (model, Cmd.none)

chatResponse : PortSocket.Id -> Chat.Line -> Model -> (Model, Cmd Msg)
chatResponse id line model =
  case line.command of
    "PING" -> 
      let _ = Debug.log "PONG" "" in
      (model, PortSocket.send id ("PONG :tmi.twitch.tv"))
    "001" -> (model, Cmd.none)
    "002" -> (model, Cmd.none)
    "003" -> (model, Cmd.none)
    "004" -> (model, Cmd.none)
    "375" -> (model, Cmd.none)
    "372" -> (model, Cmd.none)
    "376" -> 
      --let _ = Debug.log "logged in" "" in
      ( model
      , Cmd.batch
        [ PortSocket.send id "CAP REQ :twitch.tv/tags"
        , model.login
          |> Maybe.map (\channel -> PortSocket.send id ("JOIN #" ++ channel))
          |> Maybe.withDefault Cmd.none
        ]
      )
    _ ->
      let _ = Debug.log "parse" line in
      (model, Cmd.none)

reduce : (msg -> Model -> (Model, Cmd Msg)) -> msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
reduce step msg (model, cmd) =
  let
    (m2, c2) = step msg model
  in
    (m2, Cmd.batch [cmd, c2])

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize (\w h -> WindowSize (w, h))
    , Browser.Events.onAnimationFrameDelta FrameStep
    , PortSocket.receive WebSocketTest
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

fetchSelfUrl : String
fetchSelfUrl =
  "https://api.twitch.tv/helix/users"

fetchSelf : Maybe String -> Cmd Msg
fetchSelf auth =
  if auth == Nothing then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = auth
      , decoder = Helix.users
      , tagger = Self
      , url = fetchSelfUrl
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

extractHashArgument : String -> Url -> Maybe String
extractHashArgument key location =
  { location | path = "", query = location.fragment }
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
  Url.custom Url.Relative [] (createQueryString model) (Maybe.map (\token -> "access_token=" ++ token) model.auth)
