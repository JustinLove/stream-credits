module StreamCredits exposing (..)

import View exposing (Host)


import Browser
import Browser.Navigation as Navigation
import Http
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
  | Hosts (Result Http.Error (List Tmi.Host))
  | User (Result Http.Error (List Helix.User))

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , login : Maybe String
  , userId : Maybe String
  , hosts : List Host
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
    , login = mlogin
    , userId = muserId
    , hosts = []
    }
  , ( case (muserId, mlogin) of
      (Just id, Just login) -> fetchHosts id
      (Just id, Nothing) -> Cmd.batch [ fetchUserById id, fetchHosts id ]
      (Nothing, Just login) -> fetchUserByName login
      (Nothing, Nothing) -> Cmd.none
    )
  )

update msg model =
  case msg of
    UI (View.SetUsername username) ->
      ( model, fetchUserByName username )
    CurrentUrl location ->
      ( { model | location = location }, Cmd.none)
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
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

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

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
