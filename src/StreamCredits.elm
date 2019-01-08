module StreamCredits exposing (..)

import ObsStudio
import PortSocket
import Twitch.Tmi.Chat as Chat
--import Twitch.Tmi.ChatSamples as Chat
import View exposing (Host, Raid, Cheer, Sub, Follow)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Parser.Advanced as Parser
import Platform.Sub
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

twitchIrc = "wss://irc-ws.chat.twitch.tv:443"

type Msg
  = UI (View.Msg)
  | CurrentUrl Url
  | Navigate Browser.UrlRequest
  | CurrentTime Time.Posix
  | WindowSize (Int, Int)
  | Visibility Browser.Events.Visibility
  | FrameStep Float
  | Hosts (Result Http.Error (List Tmi.Host))
  | User (Result Http.Error (List Helix.User))
  | Follows (Result Http.Error (List Helix.Follow))
  | CurrentStream (Result Http.Error (List Helix.Stream))
  | SocketEvent PortSocket.Id PortSocket.Event

type ConnectionStatus
  = Disconnected
  | Connecting String
  | Connected PortSocket.Id
  | LoggedIn PortSocket.Id String
  | Joining PortSocket.Id String String
  | Joined PortSocket.Id String String

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , time : Time.Posix
  , windowWidth : Int
  , windowHeight : Int
  , visibility : Browser.Events.Visibility
  , timeElapsed : Float
  , login : Maybe String
  , userId : Maybe String
  , ircConnection : ConnectionStatus
  , hosts : List Host
  , raids : List Raid
  , cheers : Dict String Cheer
  , subs : Dict String Sub
  , follows : List Follow
  , currentFollows : List Follow
  , streamStart : Int
  }

main = Browser.application
  { init = init
  , view = View.document UI
  , update = updateWithChecks
  , subscriptions = subscriptions
  , onUrlRequest = Navigate
  , onUrlChange = CurrentUrl
  }

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags location key =
  let
    initialModel =
      { location = location
      , navigationKey = key
      , time = Time.millisToPosix 0
      , windowWidth = 480
      , windowHeight = 480
      , visibility = Browser.Events.Visible
      , timeElapsed = 0
      , login = Nothing
      , userId = Nothing
      , ircConnection = Disconnected
      , hosts = []
      , raids = []
      , cheers = Dict.empty
      , subs = Dict.empty
      , follows = []
      , currentFollows = []
      , streamStart = 0
      }
      --|> update (SocketEvent 0 (PortSocket.Message Chat.sampleResubMessage)) |> Tuple.first
      --|> update (SocketEvent 0 (PortSocket.Message Chat.sampleGiftedSubMessage)) |> Tuple.first
      --|> update (SocketEvent 0 (PortSocket.Message Chat.sampleAnonGiftedSubMessage)) |> Tuple.first
      --|> update (SocketEvent 0 (PortSocket.Message Chat.sampleBitsChatMessage)) |> Tuple.first
      --|> update (SocketEvent 0 (PortSocket.Message Chat.sampleRaidedMessage)) |> Tuple.first
    (m2, argCmds) = update (CurrentUrl location) initialModel
  in
  (m2, Cmd.batch 
    [ argCmds
    , Time.now |> Task.perform CurrentTime
    , Dom.getViewport
      |> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
      |> Task.perform WindowSize
    ]
  )

updateWithChecks msg model =
  let
    (m2,cmd2) = update msg model
    (m3,cmd3) = chatConnectionUpdate m2
  in
    (m3,Cmd.batch[cmd3, cmd2])

update msg model =
  case msg of
    UI (View.SetUsername username) ->
      ( { model | timeElapsed = 0 }, fetchUserByName username )
    CurrentUrl location ->
      let
        mlogin = extractSearchArgument "login" location
        muserId = extractSearchArgument "userId" location
      in
      ( { model
        | location = location
        , login = mlogin
        , userId = muserId
        }
      , Cmd.batch
        [ ( case (muserId, mlogin) of
          (Just id, _) -> refresh muserId
          (Nothing, Just login) -> fetchUserByName login
          (Nothing, Nothing) -> Cmd.none
          )
        ]
      )
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
    CurrentTime time ->
      ( {model | time = time}, Cmd.none)
    WindowSize (width, height) ->
      ( {model | windowWidth = width, windowHeight = height}, Cmd.none)
    Visibility visible ->
      ( {model | visibility = visible, timeElapsed = 0 }
      , if visible == Browser.Events.Visible then
          refresh model.userId
        else
          Cmd.none
      )
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
    SocketEvent id (PortSocket.Error value) ->
      let _ = Debug.log "websocket error" value in
      (model, Cmd.none)
    SocketEvent id (PortSocket.Open url) ->
      let _ = Debug.log "websocket open" id in
      ({model | ircConnection = Connected id}, PortSocket.send id ("NICK justinfan" ++ (String.fromInt (modBy 1000000 (Time.posixToMillis model.time)))))
    SocketEvent id (PortSocket.Close url) ->
      let _ = Debug.log "websocket closed" id in
      ({model | ircConnection = Disconnected}, Cmd.none)
    SocketEvent id (PortSocket.Message message) ->
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
    "JOIN" ->
      let
          user = line.prefix
            |> Maybe.map Chat.extractUserFromPrefix
            |> Maybe.withDefault (Err [])
            |> Result.withDefault "unknown"
          channel = line.params
            |> List.head
            |> Maybe.withDefault "unknown"
      in
      ({model | ircConnection = Joined id user channel}, Cmd.none)
    "PART" ->
      let
          user = line.prefix
            |> Maybe.map Chat.extractUserFromPrefix
            |> Maybe.withDefault (Err [])
            |> Result.withDefault "unknown"
      in
      ({model | ircConnection = LoggedIn id user}, Cmd.none)
    "PING" -> 
      --let _ = Debug.log "PONG" "" in
      (model, PortSocket.send id ("PONG :tmi.twitch.tv"))
    "PRIVMSG" ->
      let cheer = myCheer line in
      if cheer.amount > 0 then
        ( { model
          | cheers = Dict.update cheer.userId (\mprev ->
            case mprev of
              Just prev -> Just {prev | amount = prev.amount + cheer.amount}
              Nothing -> Just cheer
            )
            model.cheers
          }
        , Cmd.none )
      else
        (model, Cmd.none)
    "USERNOTICE" ->
      let _ = Debug.log "usernotice" line in
      let
        msgId = line.tags
          |> List.filterMap (\tag -> case tag of 
            Chat.MsgId kind -> Just kind
            _ -> Nothing
          )
          |> List.head
          |> Maybe.withDefault (Chat.UnknownNotice "missing msg-id")
      in
      case msgId of
        Chat.Raid -> 
          let raid = myRaid line in
          ( { model | raids = raid :: model.raids }, Cmd.none )
        Chat.Resub -> 
          let sub = mySub line in
          ( combineSubs sub model, Cmd.none )
        Chat.SubGift -> 
          let sub = mySub line in
          ( combineSubs sub model, Cmd.none )
        Chat.AnonSubGift -> 
          let sub = mySub line in
          ( combineSubs {sub | displayName = "anonymous"} model, Cmd.none )
        _ -> 
          (model, Cmd.none)
    "001" -> (model, Cmd.none)
    "002" -> (model, Cmd.none)
    "003" -> (model, Cmd.none)
    "004" -> (model, Cmd.none)
    "375" -> (model, Cmd.none)
    "372" -> (model, Cmd.none)
    "376" -> 
      --let _ = Debug.log "logged in" "" in
      ( {model | ircConnection = LoggedIn id (line.params |> List.head |> Maybe.withDefault "unknown")}
      , Cmd.batch
        [ PortSocket.send id "CAP REQ :twitch.tv/tags"
        , PortSocket.send id "CAP REQ :twitch.tv/commands"
        ]
      )
    _ ->
      let _ = Debug.log "parse" line in
      (model, Cmd.none)

combineSubs : Sub -> Model -> Model
combineSubs sub model =
  { model
  | subs = Dict.update sub.userId (\mprev ->
    case mprev of
      Just prev -> Just
        { prev
        | months = max prev.months sub.months
        , points = prev.points + sub.points
        }
      Nothing -> Just sub
    )
    model.subs
  }

chatConnectionUpdate : Model -> (Model, Cmd Msg)
chatConnectionUpdate model =
  case (model.ircConnection, model.login) of
    (Disconnected, Just channel) ->
      ( {model | ircConnection = Connecting twitchIrc}
      , PortSocket.connect twitchIrc
      )
    (LoggedIn id login, Just channel) ->
      ( {model | ircConnection = Joining id login channel}
      , PortSocket.send id ("JOIN #" ++ channel)
      )
    (Joining id login channel, Nothing) ->
      ( model
      , PortSocket.send id ("PART #" ++ channel)
      )
    (Joined id login channel, Nothing) ->
      ( model
      , PortSocket.send id ("PART " ++ channel)
      )
    (_, _) ->
      (model, Cmd.none)

reduce : (msg -> Model -> (Model, Cmd Msg)) -> msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
reduce step msg (model, cmd) =
  let
    (m2, c2) = step msg model
  in
    (m2, Cmd.batch [cmd, c2])

refresh : Maybe String -> Cmd Msg
refresh mUserId =
  case mUserId of
    Just id ->
      Cmd.batch
        [ fetchHosts id
        , fetchFollows id
        , fetchStreamById id
        ]
    Nothing ->
      Cmd.none

subscriptions : Model -> Platform.Sub.Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize (\w h -> WindowSize (w, h))
    , if model.visibility == Browser.Events.Visible then
        Browser.Events.onAnimationFrameDelta FrameStep
      else
        Sub.none
    , Browser.Events.onVisibilityChange Visibility
    , ObsStudio.onVisibilityChange Visibility
    , PortSocket.receive SocketEvent
    ]

myHost : Tmi.Host -> Host
myHost host =
  { hostId = host.hostId
  , hostDisplayName = host.hostDisplayName
  }

myRaid : Chat.Line -> Raid
myRaid line =
  List.foldl (\tag raid ->
      case tag of
        Chat.MsgParamDisplayName name -> {raid | displayName = name}
        Chat.UserId id -> {raid | userId = id}
        Chat.MsgParamViewerCount count -> {raid | viewerCount = count}
        _ -> raid
    ) (Raid "" "" 0) line.tags

myCheer : Chat.Line -> Cheer
myCheer line =
  List.foldl (\tag cheer ->
      case tag of
        Chat.DisplayName name -> {cheer | displayName = name}
        Chat.UserId id -> {cheer | userId = id}
        Chat.Bits amount -> {cheer | amount = amount}
        _ -> cheer
    ) (Cheer "" "" 0) line.tags

mySub : Chat.Line -> Sub
mySub line =
  List.foldl (\tag sub ->
      case tag of
        Chat.DisplayName name -> {sub | displayName = name}
        Chat.UserId id -> {sub | userId = id}
        Chat.MsgParamMonths count -> {sub | months = count}
        Chat.MsgParamSubPlan plan ->
          case plan of
            "Prime" ->
              {sub | points = 1}
            "1000" ->
              {sub | points = 1}
            "2000" ->
              {sub | points = 2}
            "3000" ->
              {sub | points = 3}
            _ ->
              {sub | points = 1}
        _ -> sub
    ) (Sub "" "" 0 0) line.tags


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
  Url.custom Url.Relative [] (createQueryString model) Nothing
