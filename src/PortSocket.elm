port module PortSocket exposing (Id, Event(..), connect, send, receive)

import Json.Decode exposing (..)
import Json.Encode

connect : String -> Cmd msg
connect = webSocketConnect

send : Id -> String -> Cmd msg
send id data =
  webSocketSend (id, data)

receive : (Id -> Event -> msg) -> Sub msg
receive tagger =
  webSocketReceive (decodeReceive >> (\(id, e) -> tagger id e))

decodeReceive : Value -> (Id, Event)
decodeReceive thing =
  decodeValue idEvent thing
    |> Result.mapError (Debug.log "websocket decode error")
    |> Result.withDefault (0, Error Json.Encode.null)

type alias Id = Int

type Event
  = Error Value
  | Open
  | Close
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
        "open" -> succeed Open
        "close" -> succeed Close
        "message" -> map Message (field "message" string)
        _ -> succeed (Error Json.Encode.null)
    )

port webSocketConnect : String -> Cmd msg
port webSocketSend : (Id, String) -> Cmd msg
port webSocketReceive : (Value -> msg) -> Sub msg
