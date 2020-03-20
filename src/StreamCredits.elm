module StreamCredits exposing (..)

import ObsStudio
import Pagination as Helix
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
import Time exposing (Posix)
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
  | CurrentTime Posix
  | WindowSize (Int, Int)
  | Visibility Browser.Events.Visibility
  | FrameStep Float
  | Hosts (Result Http.Error (List Tmi.HostingTarget))
  | User (Result Http.Error (List Helix.User))
  | Self (Result Http.Error (List Helix.User))
  | Follows (Result Http.Error (List Helix.Follow))
  | Subscriptions Int (Result Http.Error (Helix.Paginated (List Helix.Subscription)))
  | BitsLeaderboard (Result Http.Error (List Helix.BitsLeader))
  | CurrentStream (Result Http.Error (List Helix.Stream))
  | SocketEvent PortSocket.Id PortSocket.Event
  | Reconnect Posix

type ConnectionStatus
  = Disconnected
  | Connecting String Float
  | Connected PortSocket.Id
  | LoggedIn PortSocket.Id String
  | Joining PortSocket.Id String String
  | Joined PortSocket.Id String String

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , time : Posix
  , windowWidth : Int
  , windowHeight : Int
  , visibility : Browser.Events.Visibility
  , timeElapsed : Float
  , login : Maybe String
  , userId : Maybe String
  , auth : Maybe String
  , authLogin : Maybe String
  , ircConnection : ConnectionStatus
  , hosts : List Host
  , raids : List Raid
  , cheers : Dict String Cheer
  , bitsLeaders : List Cheer
  , subs : Dict String Sub
  , subscribers : List Sub
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
      , auth = Nothing
      , authLogin = Nothing
      , ircConnection = Disconnected
      , hosts = []
      , raids = []
      , cheers = Dict.empty
      , bitsLeaders = []
      , subs = Dict.empty
      , subscribers = []
      , follows = []
      , currentFollows = []
      , streamStart = 0
      }
      --|> update (Subscriptions ((Decode.decodeString Kraken.subscriptions Kraken.sampleSubscriptions) |> Result.mapError (always Http.NetworkError))) |> Tuple.first
      --|> update (BitsLeaderboard ((Decode.decodeString Helix.bitsLeaderboard Helix.sampleBitsLeaderboard) |> Result.mapError (always Http.NetworkError))) |> Tuple.first
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
    UI _ -> (model, Cmd.none)
    CurrentUrl location ->
      let
        mlogin = extractSearchArgument "login" location
        muserId = extractSearchArgument "userId" location
        mauth = extractHashArgument "access_token" location
      in
      ( { model
        | location = location
        , login = mlogin
        , userId = muserId
        , auth = mauth
        }
      , case mauth of
        Just auth ->
          Cmd.batch
            [ ( case (muserId, mlogin) of
              (Just id, _) -> refresh auth muserId
              (Nothing, Just login) -> fetchUserByName auth login
              (Nothing, Nothing) -> Cmd.none
              )
            , fetchSelf auth
            ]
        Nothing ->
          Cmd.none
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
          case model.auth of
            Just auth -> refresh auth model.userId
            Nothing -> Cmd.none
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
          , cheers =
              if (Just user.id) /= model.userId then
                Dict.empty
              else
                model.cheers
          , raids =
              if (Just user.id) /= model.userId then
                []
              else
                model.raids
          }
      in
      ( m2
      , if (Just user.id) /= model.userId then
          Cmd.batch
            [ Navigation.pushUrl m2.navigationKey (createPath m2)
            , case model.auth of
                Just auth -> refresh auth (Just user.id)
                Nothing -> Cmd.none
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
        update (User (Ok [user])) m2
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
    Subscriptions offset (Ok (Helix.Paginated cursor twitchSubscriptions)) ->
      let
        subscribers = List.map mySubscription twitchSubscriptions
      in
        ( if offset == 0 then
            { model | subscribers = subscribers }
          else
            { model | subscribers = List.append model.subscribers subscribers }
        , if List.isEmpty subscribers then
            Cmd.none
          else
            case (model.auth, model.userId) of
              (Just auth, Just id) ->
                fetchSubscriptions auth id (offset + (List.length subscribers)) (Just cursor)
              _ ->
                Cmd.none
        )
    Subscriptions _ (Err error) ->
      let _ = Debug.log "subscriptions fetch error" error in
      (model, Cmd.none)
    BitsLeaderboard (Err error) ->
      let _ = Debug.log "bits leaderboard fetch error" error in
      (model, Cmd.none)
    BitsLeaderboard (Ok twitchBitsLeaderboard) ->
      let
        leaders = List.map myBitsLeader twitchBitsLeaderboard
      in
      ( { model | bitsLeaders = leaders }
      , Cmd.none
      )
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
      case model.ircConnection of
        Disconnected ->
          (model, Cmd.none)
        Connecting _ timeout ->
          (model, Cmd.none)
        _ ->
          ( {model | ircConnection = Connecting twitchIrc 1000}
          , Cmd.none
          )
    SocketEvent id (PortSocket.Message message) ->
      --let _ = Debug.log "websocket message" message in
      case (Parser.run Chat.message message) of
        Ok lines ->
          List.foldl (reduce (chatResponse id message)) (model, Cmd.none) lines
        Err err ->
          let _ = Debug.log message err in
          (model, Cmd.none)
    Reconnect time ->
      case Debug.log "reconnect" model.ircConnection of
        Connecting url timeout ->
          ( {model | ircConnection = Connecting url (timeout*2)}
          , PortSocket.connect url
          )
        _ ->
          (model, Cmd.none)

chatResponse : PortSocket.Id -> String -> Chat.Line -> Model -> (Model, Cmd Msg)
chatResponse id message line model =
  let
    _ = line.tags
      |> List.map (\tag -> case tag of 
        Chat.UnknownTag _ _ ->
          Debug.log message tag
        _ -> tag
      )
  in
  case line.command of
    "CAP" -> (model, Cmd.none)
    "HOSTTARGET" ->
      let _ = Debug.log "hosttarget" line in
      (model, Cmd.none)
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
    "ROOMSTATE" -> (model, Cmd.none)
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
        Chat.Sub -> 
          let sub = mySub line in
          ( combineSubs sub model, Cmd.none )
        Chat.SubGift -> 
          let sub = mySub line in
          ( combineSubs sub model, Cmd.none )
        Chat.AnonSubGift -> 
          let sub = mySub line in
          ( combineSubs {sub | displayName = "anonymous"} model, Cmd.none )
        Chat.UnknownNotice name -> 
          let _ = Debug.log "unknown notice" name in
          (model, Cmd.none)
        _ -> 
          (model, Cmd.none)
    "001" -> (model, Cmd.none)
    "002" -> (model, Cmd.none)
    "003" -> (model, Cmd.none)
    "004" -> (model, Cmd.none)
    "353" -> (model, Cmd.none) --names list
    "366" -> -- end of names list
      let _ = Debug.log "joined room" (List.head (List.drop 1 line.params)) in
      (model, Cmd.none)
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
      let _ = Debug.log message line in
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
    (Disconnected, Just _) ->
      ( {model | ircConnection = Connecting twitchIrc 1000}
      , Cmd.none
      )
    (LoggedIn id login, Just channel) ->
      ( {model | ircConnection = Joining id login channel}
      , PortSocket.send id ("JOIN #" ++ channel)
      )
    (Joining id login channel, Nothing) ->
      ( {model | ircConnection = Disconnected}
      , PortSocket.close id
      )
    (Joined id login channel, Nothing) ->
      ( {model | ircConnection = Disconnected}
      , PortSocket.close id
      )
    (_, _) ->
      (model, Cmd.none)

reduce : (msg -> Model -> (Model, Cmd Msg)) -> msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
reduce step msg (model, cmd) =
  let
    (m2, c2) = step msg model
  in
    (m2, Cmd.batch [cmd, c2])

refresh : String -> Maybe String -> Cmd Msg
refresh auth mUserId =
  case mUserId of
    Just id ->
      Cmd.batch
        [ fetchHosts id
        , fetchFollows auth id
        , fetchSubscriptions auth id 0 Nothing
        , fetchBitsLeaderboard auth
        , fetchStreamById auth id
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
    , case model.ircConnection of
        Connecting _ timeout-> Time.every timeout Reconnect
        _ -> Sub.none
    ]

myHost : Tmi.HostingTarget -> Host
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

myBitsLeader : Helix.BitsLeader -> Cheer
myBitsLeader leader =
  { userId = leader.userId
  , displayName = leader.userName
  , amount = leader.score
  }

mySub : Chat.Line -> Sub
mySub line =
  List.foldl (\tag sub ->
      case tag of
        Chat.DisplayName name -> {sub | displayName = name}
        Chat.UserId id -> {sub | userId = id}
        Chat.MsgParamMonths count -> {sub | months = count}
        Chat.MsgParamCumulativeMonths count -> {sub | months = count}
        Chat.MsgParamSubPlan plan -> {sub | points = planPoints plan}
        _ -> sub
    ) (Sub "" "" 0 0) line.tags

planPoints : String -> Int
planPoints plan =
  case plan of
    "Prime" -> 1
    "1000" -> 1
    "2000" -> 2
    "3000" -> 3
    _ -> 1

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
    , expect = Http.expectJson Hosts Tmi.hostingTarget
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

fetchFollows : String -> String -> Cmd Msg
fetchFollows auth id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.follows
    , tagger = Follows
    , url = (fetchFollowsUrl id)
    }

mySubscription : Helix.Subscription -> Sub
mySubscription sub =
  { userId = sub.userId
  , displayName = sub.userName
  , months = 0
  , points = planPoints sub.tier
  }

fetchSubscriptionsUrl : String -> Maybe String -> String
fetchSubscriptionsUrl id cursor =
  "https://api.twitch.tv/helix/subscriptions/?broadcaster_id=" ++ id ++ "&first=100" ++ (cursor |> Maybe.map (\c -> "&after=" ++ c) |> Maybe.withDefault "")

fetchSubscriptions : String -> String -> Int -> Maybe String -> Cmd Msg
fetchSubscriptions auth id offset cursor =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.paginated Helix.subscriptions
    , tagger = Subscriptions offset
    , url = (fetchSubscriptionsUrl id cursor)
    }

fetchBitsLeaderboardUrl : String
fetchBitsLeaderboardUrl =
  "https://api.twitch.tv/helix/bits/leaderboard?period=week"

fetchBitsLeaderboard : String -> Cmd Msg
fetchBitsLeaderboard auth =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.bitsLeaderboard
    , tagger = BitsLeaderboard
    , url = fetchBitsLeaderboardUrl
    }

fetchUserByNameUrl : String -> String
fetchUserByNameUrl login =
  "https://api.twitch.tv/helix/users?login=" ++ login

fetchUserByName : String -> String -> Cmd Msg
fetchUserByName auth login =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.users
    , tagger = User
    , url = (fetchUserByNameUrl login)
    }

fetchSelfUrl : String
fetchSelfUrl =
  "https://api.twitch.tv/helix/users"

fetchSelf : String -> Cmd Msg
fetchSelf auth =
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

fetchStreamById : String -> String -> Cmd Msg
fetchStreamById auth id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
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
