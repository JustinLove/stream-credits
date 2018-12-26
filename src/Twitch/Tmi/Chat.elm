module Twitch.Tmi.Chat exposing
  ( Message
  , message
  , Line
  , line
  , serverName
  , command
  , params
  , sampleConnectionMessage
  , samplePingMessage
  , sampleJoinMessage
  , sampleNamesMessage
  , deadEndsToString
  )

import Char
import Parser.Advanced exposing (..)
import Set

type alias MessageParser a = Parser Context Problem a
type alias Line =
  { prefix : Maybe String
  , command : String
  , params : String
  }
type alias Message = List Line
type alias Context = String
type alias Problem = String

message : MessageParser Message
message =
  inContext "parsing an batch of IRC messages" <|
    loop [] messageStep

messageStep : Message -> MessageParser (Step Message Message)
messageStep reverseMessages =
  oneOf
    [ succeed (\m -> Loop (m :: reverseMessages))
      |= line
      |. spaces
    , succeed ()
      |> map (\_ -> Done (List.reverse reverseMessages))
    ]

line : MessageParser Line
line =
  inContext "parsing an IRC message" <|
    succeed Line
      |= prefix
      |= command
      |. spaces
      |= params
      |. symbol (Token "\r\n" "Looking for end of line")

prefix : MessageParser (Maybe String)
prefix =
  inContext "parsing a prefix" <|
    oneOf
      [ succeed Just
        |. symbol (Token ":" "Expecting line to start with :")
        |= serverName
        |. spaces
      , succeed Nothing
      ]

serverName : MessageParser String
serverName =
  inContext "parsing a server name" <|
    variable
      { start = Char.isAlphaNum
      , inner = \c -> Char.isAlphaNum c || c == '.'
      , reserved = Set.empty
      , expecting = "did not look like a domain name"
      }

command : MessageParser String
command =
  oneOf
    [ numericCommand
    , alphaCommand
    ]

numericCommand : MessageParser String
numericCommand =
  inContext "parsing a numeric command" <|
    getChompedString <|
      succeed ()
        |. chompIf Char.isDigit "expecting first digit"
        |. chompIf Char.isDigit "expecting second digit"
        |. chompIf Char.isDigit "expecting third digit"

alphaCommand : MessageParser String
alphaCommand =
  inContext "parsing a alpha command" <|
    variable
      { start = Char.isAlpha
      , inner = \c -> Char.isAlpha c
      , reserved = Set.empty
      , expecting = "alpha character"
      }

params : MessageParser String
params =
  inContext "parsing params" <|
    oneOf
      [ succeed identity
        |. token (Token ":" "Looking for param beginning")
        |= (getChompedString <|
          chompUntilEndOr "\r\n")
      , succeed identity
        |. (getChompedString <|
          chompWhile (\c -> c /= ' '))
        |. token (Token " :" "Looking for param separator")
        |= (getChompedString <|
          chompUntilEndOr "\r\n")
      ]

sampleConnectionMessage = ":tmi.twitch.tv 001 wondibot :Welcome, GLHF!\r\n:tmi.twitch.tv 002 wondibot :Your host is tmi.twitch.tv\r\n:tmi.twitch.tv 003 wondibot :This server is rather new\r\n:tmi.twitch.tv 004 wondibot :-\r\n:tmi.twitch.tv 375 wondibot :-\r\n:tmi.twitch.tv 372 wondibot :You are in a maze of twisty passages, all alike.\r\n:tmi.twitch.tv 376 wondibot :>\r\n"

samplePingMessage = "PING :tmi.twitch.tv\r\n"

sampleJoinMessage = ":wondibot!wondibot@wondibot.tmi.twitch.tv JOIN wondible\r\n"

sampleNamesMessage = ":wondibot.tmi.twitch.tv 353 wondibot = wondible :wondibot\r\n:wondibot.tmi.twitch.tv 366 wondibot wondible :End of /NAMES list\r\n"

deadEndsToString : List (DeadEnd Context Problem) -> String
deadEndsToString deadEnds =
  deadEnds
    |> List.map deadEndToString
    |> String.join "\n"

deadEndToString : DeadEnd Context Problem -> String
deadEndToString {problem, contextStack} =
  problem :: (contextStack |> List.map contextToString)
    |> String.join " while: "

contextToString : {r|context : Context} -> String
contextToString {context} =
  context
