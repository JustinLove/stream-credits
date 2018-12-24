module ChatTest exposing (..)

import Twitch.Tmi.Chat as Chat

import Parser.Advanced as Parser

import Expect exposing (Expectation)
--import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (Test, describe, test)


suite : Test
suite =
  describe "Twitch IRC Parsing"
    [ describe "connection message"
      [ test "breaks lines" <|
        \_ ->
          Chat.sampleConnectionMessage
              |> Parser.run Chat.messages
              |> Debug.log "parsed chat"
              |> resultOk (List.length >> (Expect.equal 7))
      ]
    ]


resultOk : (value -> Expectation) -> Result error value -> Expectation
resultOk expect result =
  case result of
    Ok value -> expect value
    Err error ->
      let _ = Debug.log "result error" error in
      Expect.true "result error" False


isOk : Result error value -> Expectation
isOk result =
  Expect.true "isOk" <|
    case result of
      Ok _ -> True
      Err error -> let _ = Debug.log "not ok" error in False
