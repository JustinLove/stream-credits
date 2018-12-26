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
              |> Parser.run Chat.message
              --|> Debug.log "parsed chat"
              |> resultOk (List.length >> (Expect.equal 7))
      , test "single line" <|
        \_ ->
          Chat.samplePingMessage
              |> Parser.run Chat.message
              --|> Debug.log "parsed chat"
              |> resultOk (List.length >> (Expect.equal 1))
      ]
    , describe "connection line"
      [ test "line one" <|
        \_ ->
          ":tmi.twitch.tv 001 wondibot :Welcome, GLHF!\r\n"
              |> Parser.run Chat.line
              --|> Debug.log "parsed line"
              |> Expect.equal (Ok (Chat.Line (Just "tmi.twitch.tv") "001" "Welcome, GLHF!"))
      ]
    , test "ping line" <|
        \_ ->
          Chat.samplePingMessage
              |> Parser.run Chat.line
              --|> Debug.log "parsed line"
              |> Expect.equal (Ok (Chat.Line Nothing "PING" "tmi.twitch.tv"))
    , describe "serverName"
      [ test "domain name" <|
        \_ ->
          "tmi.twitch.tv"
              |> Parser.run Chat.serverName
              --|> Debug.log "parsed serverName"
              |> Expect.equal (Ok "tmi.twitch.tv")
      ]
    , describe "command"
      [ test "numeric" <|
        \_ ->
          "001"
              |> Parser.run Chat.command
              --|> Debug.log "parsed command"
              |> Expect.equal (Ok "001")
      , test "alpha" <|
        \_ ->
          "PING"
              |> Parser.run Chat.command
              --|> Debug.log "parsed command"
              |> Expect.equal (Ok "PING")
      ]
    , describe "params"
      [ test "welcome message" <|
        \_ ->
          "wondibot :Welcome, GLHF!"
              |> Parser.run Chat.params
              --|> Debug.log "parsed params"
              |> Expect.equal (Ok "Welcome, GLHF!")
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
