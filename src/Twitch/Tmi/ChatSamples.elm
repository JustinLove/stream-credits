module Twitch.Tmi.ChatSamples exposing
  ( sampleConnectionMessage
  , samplePingMessage
  , sampleJoinMessage
  , sampleNamesMessage
  , sampleChatMessage
  , sampleTaggedChatMessage
  , sampleEmoteChatMessage
  , sampleEmoteRepeatedChatMessage
  , sampleBitsChatMessage
  , sampleResubMessage
  )

sampleConnectionMessage = ":tmi.twitch.tv 001 wondibot :Welcome, GLHF!\r\n:tmi.twitch.tv 002 wondibot :Your host is tmi.twitch.tv\r\n:tmi.twitch.tv 003 wondibot :This server is rather new\r\n:tmi.twitch.tv 004 wondibot :-\r\n:tmi.twitch.tv 375 wondibot :-\r\n:tmi.twitch.tv 372 wondibot :You are in a maze of twisty passages, all alike.\r\n:tmi.twitch.tv 376 wondibot :>\r\n"

samplePingMessage = "PING :tmi.twitch.tv\r\n"

sampleJoinMessage = ":wondibot!wondibot@wondibot.tmi.twitch.tv JOIN #wondible\r\n"

sampleNamesMessage = ":wondibot.tmi.twitch.tv 353 wondibot = #wondible :wondibot\r\n:wondibot.tmi.twitch.tv 366 wondibot #wondible :End of /NAMES list\r\n"

sampleChatMessage = ":wondible!wondible@wondible.tmi.twitch.tv PRIVMSG #wondible :test\r\n"

sampleTaggedChatMessage = "@badges=broadcaster/1;color=#1E90FF;display-name=wondible;emotes=;flags=;id=036fe963-8707-44a1-8fb2-e1412343825d;mod=0;room-id=56623426;subscriber=0;tmi-sent-ts=1546013301508;turbo=0;user-id=56623426;user-type= :wondible!wondible@wondible.tmi.twitch.tv PRIVMSG #wondible :test\r\n"

sampleEmoteChatMessage = "@badges=;color=#1E90FF;display-name=Stay_Hydrated_Bot;emotes=869375:0-11/1:94-95;flags=;id=15992f17-5504-4879-80df-2c81b55b3422;mod=0;room-id=56623426;subscriber=0;tmi-sent-ts=1546015898754;turbo=0;user-id=183484964;user-type= :stay_hydrated_bot!stay_hydrated_bot@stay_hydrated_bot.tmi.twitch.tv PRIVMSG #wondible :stayhyBottle [reminder] Live for 2 hours. Total water consumed should be at least 8oz (240mL) :)\r\n"

sampleEmoteRepeatedChatMessage = "@badges=global_mod/1,turbo/1;color=#0D4200;display-name=dallas;emotes=25:0-4,12-16/1902:6-10;id=b34ccfc7-4977-403a-8a94-33c6bac34fb8;mod=0;room-id=1337;subscriber=0;tmi-sent-ts=1507246572675;turbo=1;user-id=1337;user-type=global_mod :ronni!ronni@ronni.tmi.twitch.tv PRIVMSG #dallas :Kappa Keepo Kappa\r\n"

sampleBitsChatMessage = "@badges=staff/1,bits/1000;bits=100;color=;display-name=dallas;emotes=;id=b34ccfc7-4977-403a-8a94-33c6bac34fb8;mod=0;room-id=1337;subscriber=0;tmi-sent-ts=1507246572675;turbo=1;user-id=1337;user-type=staff :ronni!ronni@ronni.tmi.twitch.tv PRIVMSG #dallas :cheer100\r\n"

sampleResubMessage = "@badges=staff/1,broadcaster/1,turbo/1;color=#008000;display-name=ronni;emotes=;id=db25007f-7a18-43eb-9379-80131e44d633;login=ronni;mod=0;msg-id=resub;msg-param-months=6;msg-param-sub-plan=Prime;msg-param-sub-plan-name=Prime;room-id=1337;subscriber=1;system-msg=ronni\\shas\\ssubscribed\\sfor\\s6\\smonths!;tmi-sent-ts=1507246572675;turbo=1;user-id=1337;user-type=staff :tmi.twitch.tv USERNOTICE #dallas :Great stream -- keep it up!\r\n"
