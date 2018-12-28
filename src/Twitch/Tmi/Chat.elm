module Twitch.Tmi.Chat exposing
  ( Message
  , message
  , Line
  , line
  , Tag(..)
  , prefix
  , command
  , params
  , sampleConnectionMessage
  , samplePingMessage
  , sampleJoinMessage
  , sampleNamesMessage
  , sampleChatMessage
  , sampleTaggedChatMessage
  , deadEndsToString
  )

import Char
import Parser.Advanced exposing (..)
import Set

type alias MessageParser a = Parser Context Problem a
type alias Line =
  { tags : List Tag
  , prefix : Maybe String
  , command : String
  , params : List String
  }
type Tag
  = Badges String
  | Color String
  | DisplayName String
  | Emotes String
  | Flags String
  | MessageId String
  | Mod Bool
  | RoomId String
  | Subscriber Bool
  | TmiSentTs String
  | Turbo Bool
  | UserId String
  | UserType String
  | UnknownTag String String
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
      |. end "unparsed trailing characters in message"
      |> map (\_ -> Done (List.reverse reverseMessages))
    ]

line : MessageParser Line
line =
  inContext "parsing an IRC message" <|
    succeed Line
      |= optionalTags
      |= optionalPrefix
      |= command
      |= params
      |. symbol (Token "\r\n" "Looking for end of line")

optionalTags : MessageParser (List Tag)
optionalTags =
  inContext "parsing tags" <|
    oneOf
      [ sequence
        { start = (Token "@" "Expecting line to start with @")
        , separator = (Token ";" "Expecting tags to be separated with ;")
        , end = (Token " " "Expecting tag list to be terminated with space")
        , spaces = succeed ()
        , item = tag
        , trailing = Forbidden
        }
      , succeed []
      ]

tag : MessageParser Tag
tag =
  inContext "parsing tag" <|
    oneOf
      [ succeed Badges
        |. tagName "badges"
        |= tagValue
      , succeed Color
        |. tagName "color"
        |= tagValue
      , succeed DisplayName
        |. tagName "display-name"
        |= tagValue
      , succeed Emotes
        |. tagName "emotes"
        |= tagValue
      , succeed Flags
        |. tagName "flags"
        |= tagValue
      , succeed MessageId
        |. tagName "id"
        |= tagValue
      , succeed Mod
        |. tagName "mod"
        |= tagBool
      , succeed RoomId
        |. tagName "room-id"
        |= tagValue
      , succeed Subscriber
        |. tagName "subscriber"
        |= tagBool
      , succeed TmiSentTs
        |. tagName "tmi-sent-ts"
        |= tagValue
      , succeed Turbo
        |. tagName "turbo"
        |= tagBool
      , succeed UserId
        |. tagName "user-id"
        |= tagValue
      , succeed UserType
        |. tagName "user-type"
        |= tagValue
      , succeed UnknownTag
        |= unknownTag
        |. equals
        |= tagValue
      ]

tagName : String -> MessageParser ()
tagName name =
  keyword (Token name ("Looking for a tag " ++ name)) |. equals

unknownTag : MessageParser String
unknownTag =
  inContext "parsing unknown tag" <|
    (getChompedString <|
      succeed ()
        |. chompWhile (\c -> c /= '=' && c /= ';' && c /= ' ')
    )

equals : MessageParser ()
equals =
  symbol (Token "=" "Expecting = seperator")

tagValue : MessageParser String
tagValue =
  inContext "parsing tag value" <|
    (getChompedString <|
      succeed ()
        |. chompWhile (\c -> c /= ';' && c /= ' ')
    )

tagBool : MessageParser Bool
tagBool =
  inContext "parsing tag bool" <|
    oneOf
      [ succeed False
        |. token (Token "0" "Looking for 0")
      , succeed True
        |. token (Token "1" "Looking for 1")
      ]

optionalPrefix : MessageParser (Maybe String)
optionalPrefix =
  inContext "parsing a prefix" <|
    oneOf
      [ succeed Just
        |. symbol (Token ":" "Expecting line to start with :")
        |= prefix
        |. spaces
      , succeed Nothing
      ]

prefix : MessageParser String
prefix =
  inContext "parsing prefix" <|
    oneOf
      [ backtrackable fullyQualifiedUser
      , host
      ]

fullyQualifiedUser : MessageParser String
fullyQualifiedUser =
  inContext "fully qualified user" <|
    getChompedString <|
      succeed ()
        |. variable
          { start = Char.isAlphaNum
          , inner = \c -> Char.isAlphaNum c
          , reserved = Set.empty
          , expecting = "did not look like a nick"
          }
        |. symbol (Token "!" "looking for !")
        |. variable
          { start = Char.isAlphaNum
          , inner = \c -> Char.isAlphaNum c
          , reserved = Set.empty
          , expecting = "did not look like a user name"
          }
        |. symbol (Token "@" "looking for @")
        |. host

host : MessageParser String
host =
  inContext "parsing a host" <|
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

params : MessageParser (List String)
params =
  inContext "parsing params" <|
    loop [] paramStep

paramStep : List String -> MessageParser (Step (List String) (List String))
paramStep reverseParams =
  succeed identity
    |. chompWhile (\c -> c == ' ')
    |= oneOf
      [ succeed (\m -> Done (List.reverse (m :: reverseParams)))
        |. token (Token ":" "looking for : to begin trailing")
        |= (getChompedString <|
          chompUntilEndOr "\r\n")
      , succeed (\m -> Loop (m :: reverseParams))
        |= (getChompedString <|
            succeed ()
              |. chompIf middle "Could not find start of parameter"
              |. chompWhile middle
          )
      , succeed ()
        |> map (\_ -> Done (List.reverse reverseParams))
      ]

middle : Char -> Bool
middle c =
  c /= ' ' && c /= '\r' && c /= '\n'

sampleConnectionMessage = ":tmi.twitch.tv 001 wondibot :Welcome, GLHF!\r\n:tmi.twitch.tv 002 wondibot :Your host is tmi.twitch.tv\r\n:tmi.twitch.tv 003 wondibot :This server is rather new\r\n:tmi.twitch.tv 004 wondibot :-\r\n:tmi.twitch.tv 375 wondibot :-\r\n:tmi.twitch.tv 372 wondibot :You are in a maze of twisty passages, all alike.\r\n:tmi.twitch.tv 376 wondibot :>\r\n"

samplePingMessage = "PING :tmi.twitch.tv\r\n"

sampleJoinMessage = ":wondibot!wondibot@wondibot.tmi.twitch.tv JOIN #wondible\r\n"

sampleNamesMessage = ":wondibot.tmi.twitch.tv 353 wondibot = #wondible :wondibot\r\n:wondibot.tmi.twitch.tv 366 wondibot #wondible :End of /NAMES list\r\n"

sampleChatMessage = ":wondible!wondible@wondible.tmi.twitch.tv PRIVMSG #wondible :test\r\n"

sampleTaggedChatMessage = "@badges=broadcaster/1;color=#1E90FF;display-name=wondible;emotes=;flags=;id=036fe963-8707-44a1-8fb2-e1412343825d;mod=0;room-id=56623426;subscriber=0;tmi-sent-ts=1546013301508;turbo=0;user-id=56623426;user-type= :wondible!wondible@wondible.tmi.twitch.tv PRIVMSG #wondible :test\r\n"

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
