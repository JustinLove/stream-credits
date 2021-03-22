port module Log exposing (debug, log, info, warn, error, decodeError, httpError)

import Http
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode

debug = commandWithData "debug"
log = command "log"
info = command "info"
warn = command "warn"
error = commandWithData "error"

decodeError : String -> Decode.Error -> Cmd msg
decodeError note err =
  err
    |> Decode.errorToString
    |> Encode.string
    |> commandWithData "error" note

httpError : String -> Http.Error -> Cmd msg
httpError note err =
  (case err of
    Http.BadUrl url ->
      Encode.object
        [ ("error", Encode.string "BadUrl")
        , ("url", Encode.string url)
        ]
    Http.Timeout ->
      Encode.object
        [ ("error", Encode.string "Timeout")
        ]
    Http.NetworkError ->
      Encode.object
        [ ("error", Encode.string "NetworkError")
        ]
    Http.BadStatus status ->
      Encode.object
        [ ("error", Encode.string "BadStatus")
        , ("status", Encode.int status)
        ]
    Http.BadBody message ->
      Encode.object
        [ ("error", Encode.string "BadBody")
        , ("message", Encode.string message)
        ]
  )
    |> commandWithData "error" note

command : String -> String -> Cmd msg
command kind note =
  Encode.object
    [ ("kind", Encode.string kind)
    , ("note", Encode.string note)
    ]
    |> logCommand

commandWithData : String -> String -> Encode.Value -> Cmd msg
commandWithData kind note v =
  Encode.object
    [ ("kind", Encode.string kind)
    , ("note", Encode.string note)
    , ("value", v)
    ]
    |> logCommand

port logCommand : Value -> Cmd msg
