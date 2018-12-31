module Twitch.Tmi.Chat exposing
  ( Message
  , message
  , Line
  , line
  , Tag(..)
  , Emote(..)
  , CharacterRange
  , prefix
  , command
  , params
  , sampleConnectionMessage
  , samplePingMessage
  , sampleJoinMessage
  , sampleNamesMessage
  , sampleChatMessage
  , sampleTaggedChatMessage
  , sampleEmoteChatMessage
  , sampleEmoteRepeatedChatMessage
  , sampleBitsChatMessage
  , deadEndsToString
  )

import Char
import Parser.Advanced exposing (..)
import Set
import Time

type alias MessageParser a = Parser Context Problem a
type alias Line =
  { tags : List Tag
  , prefix : Maybe String
  , command : String
  , params : List String
  }
type Tag
  = Badges (List String)
  | Bits Int
  | Color String
  | DisplayName String
  | Emotes (List Emote)
  | Flags String
  | MessageId String
  | Mod Bool
  | RoomId String
  | Subscriber Bool
  | TmiSentTs Time.Posix
  | Turbo Bool
  | UserId String
  | UserType String
  | UnknownTag String String
type Emote = Emote String (List CharacterRange)
type alias CharacterRange = (Int,Int)
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
        |= tagBadgeList
      , succeed Bits
        |. tagName "bits"
        |= int "Expecting Int" "Invalid Int"
      , succeed Color
        |. tagName "color"
        |= tagValue
      , succeed DisplayName
        |. tagName "display-name"
        |= tagValue
      , succeed Emotes
        |. tagName "emotes"
        |= tagEmoteList
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
        |= tagTimestamp
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

tagTimestamp : MessageParser Time.Posix
tagTimestamp =
  inContext "parsing tag timestamp" <|
    map Time.millisToPosix <|
    int "Expecting timestamp" "Invalid number"

tagBadgeList : MessageParser (List String)
tagBadgeList =
  inContext "parsing badge list" <|
    loop [] tagBadgeListStep

tagBadgeListStep : List String -> MessageParser (Step (List String) (List String))
tagBadgeListStep reverseBadges =
  oneOf
    [ succeed (\m -> Loop (m :: reverseBadges))
      |. symbol (Token "," "Expecting badge seperator")
      |= badge
    , succeed (\m -> Loop (m :: reverseBadges))
      |= badge
    , succeed ()
      |> map (\_ -> Done (List.reverse reverseBadges))
    ]

badge : MessageParser String
badge =
  inContext "parsing badge value" <|
    variable
      { start = badgeCharacter
      , inner = badgeCharacter
      , reserved = Set.empty
      , expecting = "did not look like badge"
      }

badgeCharacter : Char -> Bool
badgeCharacter c =
  c /= ',' && c /= ';' && c /= ' '

tagEmoteList : MessageParser (List Emote)
tagEmoteList =
  inContext "parsing emote list" <|
    loop [] tagEmoteListStep

tagEmoteListStep : List Emote -> MessageParser (Step (List Emote) (List Emote))
tagEmoteListStep reverseEmotes =
  oneOf
    [ succeed (\m -> Loop (m :: reverseEmotes))
      |. symbol (Token "/" "Expecting emote seperator")
      |= emote
    , succeed (\m -> Loop (m :: reverseEmotes))
      |= emote
    , succeed ()
      |> map (\_ -> Done (List.reverse reverseEmotes))
    ]

emote : MessageParser Emote
emote =
  inContext "parsing emote" <|
    succeed Emote
      |= numericId
      |. symbol (Token ":" "expecting a : between emote id and locatoins")
      |= characterRangeList

numericId : MessageParser String
numericId =
  variable
    { start = Char.isDigit
    , inner = Char.isDigit
    , reserved = Set.empty
    , expecting = "did not look like a numeric id"
    }

characterRangeList : MessageParser (List CharacterRange)
characterRangeList =
  inContext "parsing character range list" <|
    loop [] characterRangeListStep

characterRangeListStep : List CharacterRange -> MessageParser (Step (List CharacterRange) (List CharacterRange))
characterRangeListStep reverseRanges =
  oneOf
    [ succeed (\m -> Loop (m :: reverseRanges))
      |. symbol (Token "," "Expecting range seperator")
      |= characterRange
    , succeed (\m -> Loop (m :: reverseRanges))
      |= characterRange
    , succeed ()
      |> map (\_ -> Done (List.reverse reverseRanges))
    ]

characterRange : MessageParser (Int,Int)
characterRange =
  inContext "character range" <|
    succeed Tuple.pair
      |= int "Expecting Int" "Invalid Number"
      |. symbol (Token "-" "expecting - between character start and end")
      |= int "Expecting Int" "Invalid Number"

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
        |. nick
        |. symbol (Token "!" "looking for !")
        |. user
        |. symbol (Token "@" "looking for @")
        |. host

nick : MessageParser String
nick =
  variable
    { start = Char.isAlphaNum
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.empty
    , expecting = "did not look like a user name or nick"
    }

user : MessageParser String
user = nick

host : MessageParser String
host =
  inContext "parsing a host" <|
    variable
      { start = Char.isAlphaNum
      , inner = \c -> Char.isAlphaNum c || c == '.' || c == '_'
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

sampleEmoteChatMessage = "@badges=;color=#1E90FF;display-name=Stay_Hydrated_Bot;emotes=869375:0-11/1:94-95;flags=;id=15992f17-5504-4879-80df-2c81b55b3422;mod=0;room-id=56623426;subscriber=0;tmi-sent-ts=1546015898754;turbo=0;user-id=183484964;user-type= :stay_hydrated_bot!stay_hydrated_bot@stay_hydrated_bot.tmi.twitch.tv PRIVMSG #wondible :stayhyBottle [reminder] Live for 2 hours. Total water consumed should be at least 8oz (240mL) :)\r\n"

sampleEmoteRepeatedChatMessage = "@badges=global_mod/1,turbo/1;color=#0D4200;display-name=dallas;emotes=25:0-4,12-16/1902:6-10;id=b34ccfc7-4977-403a-8a94-33c6bac34fb8;mod=0;room-id=1337;subscriber=0;tmi-sent-ts=1507246572675;turbo=1;user-id=1337;user-type=global_mod :ronni!ronni@ronni.tmi.twitch.tv PRIVMSG #dallas :Kappa Keepo Kappa\r\n"

sampleBitsChatMessage = "@badges=staff/1,bits/1000;bits=100;color=;display-name=dallas;emotes=;id=b34ccfc7-4977-403a-8a94-33c6bac34fb8;mod=0;room-id=1337;subscriber=0;tmi-sent-ts=1507246572675;turbo=1;user-id=1337;user-type=staff :ronni!ronni@ronni.tmi.twitch.tv PRIVMSG #dallas :cheer100\r\n"

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
