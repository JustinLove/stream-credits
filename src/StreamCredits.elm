module StreamCredits exposing (..)

import View

import Browser
import Browser.Navigation as Navigation
import Url exposing (Url)

type Msg
  = UI (View.Msg)
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
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
  ( { location = location
    , navigationKey = key
    }
  , Cmd.none
  )

update msg model =
  case msg of
    UI (View.None) ->
      (model, Cmd.none)
    CurrentUrl location ->
      ( { model | location = location }, Cmd.none)
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
    Navigate (Browser.External url) ->
      (model, Navigation.load url)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
