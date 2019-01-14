port module PortSocket exposing (Id, Event(..), connect, close, send, receive)

import Json.Decode exposing (..)
import Json.Encode as Encode

connect : String -> Cmd msg
connect address =
  Encode.object
    [ ("kind", Encode.string "connect")
    , ("address", Encode.string address)
    ]
    |> webSocketCommand

close : Int -> Cmd msg
close id =
  Encode.object
    [ ("kind", Encode.string "close")
    , ("id", Encode.int id)
    ]
    |> webSocketCommand

send : Id -> String -> Cmd msg
send id data =
  Encode.object
    [ ("kind", Encode.string "send")
    , ("id", Encode.int id)
    , ("data", Encode.string data)
    ]
    |> webSocketCommand

receive : (Id -> Event -> msg) -> Sub msg
receive tagger =
  webSocketReceive (decodeReceive >> (\(id, e) -> tagger id e))

decodeReceive : Value -> (Id, Event)
decodeReceive thing =
  decodeValue idEvent thing
    |> Result.mapError (Debug.log "websocket decode error")
    |> Result.withDefault (0, Error Encode.null)

type alias Id = Int

type Event
  = Error Value
  | Open String
  | Close String
  | Message String

idEvent : Decoder (Id, Event)
idEvent =
  map2 Tuple.pair
    (field "id" int)
    event

event : Decoder Event
event =
  (field "kind" string)
    |> andThen (\kind ->
      case kind of
        "error" -> map Error (field "error" value)
        "open" -> map Open (field "url" string)
        "close" -> map Close (field "url" string)
        "message" -> map Message (field "message" string)
        _ -> succeed (Error Encode.null)
    )

port webSocketCommand : Value -> Cmd msg
port webSocketReceive : (Value -> msg) -> Sub msg
