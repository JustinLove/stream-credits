module Twitch.Tmi.Chat exposing (messages, deadEndsToString)

import Parser.Advanced exposing (..)
import Char

type alias MessageParser a = Parser String String a

messages : MessageParser (List String)
messages =
  inContext "parsing an batch of IRC messages" <|
    loop [] messageStep

messageStep : List String -> Parser String String (Step (List String) (List String))
messageStep reverseMessages =
  oneOf
    [ succeed (\m -> Loop (m :: reverseMessages))
      |= message
      |. spaces
    , succeed ()
      |> map (\_ -> Done (List.reverse reverseMessages))
    ]

message : Parser String String String
message =
  inContext "parsing an IRC message" <|
    getChompedString <|
      chompUntil (Token "\r\n" "Looking for end of line")

sampleConnectionMessage = ":tmi.twitch.tv 001 wondibot :Welcome, GLHF!\r\n:tmi.twitch.tv 002 wondibot :Your host is tmi.twitch.tv\r\n:tmi.twitch.tv 003 wondibot :This server is rather new\r\n:tmi.twitch.tv 004 wondibot :-\r\n:tmi.twitch.tv 375 wondibot :-\r\n:tmi.twitch.tv 372 wondibot :You are in a maze of twisty passages, all alike.\r\n:tmi.twitch.tv 376 wondibot :>\r\n"

deadEndsToString : List (DeadEnd String String) -> String
deadEndsToString deadEnds =
  deadEnds
    |> List.map deadEndToString
    |> String.join "\n"

deadEndToString : DeadEnd String String -> String
deadEndToString {problem, contextStack} =
  problem :: (contextStack |> List.map contextToString)
    |> String.join " while: "

contextToString : {r|context : String} -> String
contextToString {context} =
  context
