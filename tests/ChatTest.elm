module ChatTest exposing (..)

import Twitch.Tmi.Chat as Chat

import Parser.Advanced as Parser
import Time

import Expect exposing (Expectation)
--import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (Test, describe, test)


suite : Test
suite =
  describe "Twitch IRC Parsing"
    [ describe "messages"
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
      , test "names message" <|
        \_ ->
          Chat.sampleNamesMessage
              |> Parser.run Chat.message
              --|> Debug.log "parsed chat"
              |> resultOk (List.length >> (Expect.equal 2))
      ]
    , describe "line types"
      [ test "connection line" <|
        \_ ->
          ":tmi.twitch.tv 001 wondibot :Welcome, GLHF!\r\n"
              |> Parser.run Chat.line
              |> Expect.equal (Ok (Chat.Line [] (Just "tmi.twitch.tv") "001" ["wondibot", "Welcome, GLHF!"]))
      , test "ping line" <|
          \_ ->
            Chat.samplePingMessage
                |> Parser.run Chat.line
                |> Expect.equal (Ok (Chat.Line [] Nothing "PING" ["tmi.twitch.tv"]))
      , test "join line" <|
          \_ ->
            Chat.sampleJoinMessage
                |> Parser.run Chat.line
                |> Expect.equal (Ok (Chat.Line [] (Just "wondibot!wondibot@wondibot.tmi.twitch.tv") "JOIN" ["#wondible"]))
      , test "chat line" <|
          \_ ->
            Chat.sampleChatMessage
                |> Parser.run Chat.line
                |> Expect.equal (Ok (Chat.Line [] (Just "wondible!wondible@wondible.tmi.twitch.tv") "PRIVMSG" ["#wondible", "test"]))
      , test "tagged chat line" <|
          \_ ->
            Chat.sampleTaggedChatMessage
                |> Parser.run Chat.line
                |> Expect.equal (Ok (Chat.Line
                  ( [ Chat.Badges "broadcaster/1"
                    , Chat.Color "#1E90FF"
                    , Chat.DisplayName "wondible"
                    , Chat.Emotes ""
                    , Chat.Flags ""
                    , Chat.MessageId "036fe963-8707-44a1-8fb2-e1412343825d"
                    , Chat.Mod False
                    , Chat.RoomId "56623426"
                    , Chat.Subscriber False
                    , Chat.TmiSentTs (Time.millisToPosix 1546013301508)
                    , Chat.Turbo False
                    , Chat.UserId "56623426"
                    , Chat.UserType ""
                    ]
                  )
                  (Just "wondible!wondible@wondible.tmi.twitch.tv")
                  "PRIVMSG"
                  ["#wondible", "test"])
                )
      ]
    , describe "prefix"
      [ test "domain name" <|
        \_ ->
          "tmi.twitch.tv"
              |> Parser.run Chat.prefix
              |> Expect.equal (Ok "tmi.twitch.tv")
      , test "user" <|
        \_ ->
          "wondibot!wondibot@wondibot.tmi.twitch.tv"
              |> Parser.run Chat.prefix
              |> Expect.equal (Ok "wondibot!wondibot@wondibot.tmi.twitch.tv")
      ]
    , describe "command"
      [ test "numeric" <|
        \_ ->
          "001"
              |> Parser.run Chat.command
              |> Expect.equal (Ok "001")
      , test "alpha" <|
        \_ ->
          "PING"
              |> Parser.run Chat.command
              |> Expect.equal (Ok "PING")
      ]
    , describe "params"
      [ test "two parts" <|
        \_ ->
          "wondibot :Welcome, GLHF!"
              |> Parser.run Chat.params
              |> Expect.equal (Ok ["wondibot", "Welcome, GLHF!"])
      , test "colon only" <|
        \_ ->
          ":tmi.twitch.tv"
              |> Parser.run Chat.params
              |> Expect.equal (Ok ["tmi.twitch.tv"])
      , test "no colon only" <|
        \_ ->
          "wondible"
              |> Parser.run Chat.params
              |> Expect.equal (Ok ["wondible"])
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
