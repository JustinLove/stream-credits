module Decode exposing
  ( hosts
  , User
  , users
  , follows
  , subs
  , bitsLeaderboard
  , streamTimes
  )

import View exposing (Cheer, Sub, Follow)

import Twitch.Helix as Helix exposing (UserId)
import Twitch.Kraken.Host as Host
import Twitch.Helix.User as User
import Twitch.Helix.Follow as Follow
import Twitch.Helix.Subscription as Subscription
import Twitch.Helix.BitsLeaderboard as BitsLeaderboard
import Twitch.Helix.Stream as Stream

import Json.Decode exposing (..)
import Time

hosts : Decoder (List UserId)
hosts = Host.response Host.hostId

type alias User =
  { id : UserId
  , login : String
  , displayName : String
  }

users : Decoder (List User)
users = User.response user

user : Decoder User
user =
  map3 User
    User.id
    User.login
    User.displayName

follows : Decoder (List Follow)
follows = Follow.response follow

follow : Decoder Follow
follow =
  map2 Follow
    Follow.fromName
    (map Time.posixToMillis Follow.followedAt)

subs : Decoder (List Sub)
subs = Follow.response sub

sub : Decoder Sub
sub =
  map4 Sub
    Subscription.userId
    Subscription.userName
    (succeed 0)
    (map planPoints Subscription.tier)

planPoints : String -> Int
planPoints plan =
  case plan of
    "Prime" -> 1
    "1000" -> 1
    "2000" -> 2
    "3000" -> 3
    _ -> 1

bitsLeaderboard : Decoder (List Cheer)
bitsLeaderboard = BitsLeaderboard.response bitsLeader

bitsLeader : Decoder Cheer
bitsLeader =
  map3 Cheer
    BitsLeaderboard.userId
    BitsLeaderboard.userName
    BitsLeaderboard.score

streamTimes : Decoder (List Int)
streamTimes = Stream.response (map Time.posixToMillis Stream.startedAt)
