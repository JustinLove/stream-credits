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
  | BanDuration Int
  | Bits Int
  | Color String
  | DisplayName String
  | Emotes (List Emote)
  | Flags String
  | Login String
  | MessageId String
  | Mod Bool
  | MsgId String
  | MsgParamDisplayName String
  | MsgParamLogin String
  | MsgParamMonths Int
  | MsgParamRecipientDisplayName String
  | MsgParamRecipientId String
  | MsgParamRecipientUserName String
  | MsgParamRitualName String
  | MsgParamSubPlan String
  | MsgParamSubPlanName String
  | MsgParamViewerCount Int
  | RoomId String
  | Subscriber Bool
  | SystemMsg String
  | TargetUserId String
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
      , succeed BanDuration
        |. tagName "ban-duration"
        |= int "Expecting Int" "Invalid Int"
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
      , succeed Login
        |. tagName "login"
        |= tagValue
      , succeed MessageId
        |. tagName "id"
        |= tagValue
      , succeed Mod
        |. tagName "mod"
        |= tagBool
      , succeed MsgId
        |. tagName "msg-id"
        |= tagValue
      , succeed MsgParamDisplayName
        |. tagName "msg-param-displayName"
        |= tagValue
      , succeed MsgParamLogin
        |. tagName "msg-param-login"
        |= tagValue
      , succeed MsgParamMonths
        |. tagName "msg-param-months"
        |= int "Expecting Int" "Invalid Int"
      , succeed MsgParamRecipientDisplayName
        |. tagName "msg-param-recipient-display-name"
        |= tagValue
      , succeed MsgParamRecipientId
        |. tagName "msg-param-recipient-id"
        |= tagValue
      , succeed MsgParamRecipientUserName
        |. tagName "msg-param-recipient-user-name"
        |= tagValue
      , succeed MsgParamRitualName
        |. tagName "msg-param-ritual-name"
        |= tagValue
      , succeed MsgParamSubPlanName -- backward because of substring
        |. tagName "msg-param-sub-plan-name"
        |= tagEscapedString
      , succeed MsgParamSubPlan
        |. tagName "msg-param-sub-plan"
        |= tagValue
      , succeed MsgParamViewerCount
        |. tagName "msg-param-viewerCount"
        |= int "Expecting Int" "Invalid Int"
      , succeed RoomId
        |. tagName "room-id"
        |= tagValue
      , succeed Subscriber
        |. tagName "subscriber"
        |= tagBool
      , succeed SystemMsg
        |. tagName "system-msg"
        |= tagEscapedString
      , succeed TargetUserId
        |. tagName "target-user-id"
        |= tagValue
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

tagEscapedString : MessageParser String
tagEscapedString =
  inContext "parsing escaped tag string" <|
    loop [] tagEscapedStringStep

tagEscapedStringStep : List String -> MessageParser (Step (List String) String)
tagEscapedStringStep reverseChunks =
  oneOf
    [ succeed (\m -> Loop (m :: reverseChunks))
      |. symbol (Token "\\" "Expecting backslash")
      |= oneOf
        [ map (always " ") (token (Token "s" "looking for escaped space"))
        , map (always "\n") (token (Token "n" "looking for escaped newline"))
        ]
    , succeed (\m -> Loop (m :: reverseChunks))
        |= (getChompedString <|
            succeed ()
              |. chompIf unescapedCharacter "looking for unescaped characters"
              |. chompWhile unescapedCharacter
            )
    , succeed ()
      |> map (\_ -> Done (String.join "" (List.reverse reverseChunks)))
    ]

unescapedCharacter : Char -> Bool
unescapedCharacter c =
  c /= '\\' && c /= ';' && c /= ' '

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
    unbracketedList badge ","

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
    unbracketedList emote "/"

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
    unbracketedList characterRange ","

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

unbracketedList : MessageParser a -> String -> MessageParser (List a)
unbracketedList itemParser separator =
  inContext "parsing unbrackted list" <|
    loop [] (unbracketedListStep itemParser separator)

unbracketedListStep : MessageParser a -> String -> List a -> MessageParser (Step (List a) (List a))
unbracketedListStep itemParser separator reverseItems =
  oneOf
    [ succeed (\m -> Loop (m :: reverseItems))
      |. symbol (Token separator "Expecting list seperator")
      |= itemParser
    , succeed (\m -> Loop (m :: reverseItems))
      |= itemParser
    , succeed ()
      |> map (\_ -> Done (List.reverse reverseItems))
    ]

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
