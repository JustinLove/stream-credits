module Twitch.Tmi.ChatSamples exposing (..)

sampleConnectionMessage = ":tmi.twitch.tv 001 wondibot :Welcome, GLHF!\r\n:tmi.twitch.tv 002 wondibot :Your host is tmi.twitch.tv\r\n:tmi.twitch.tv 003 wondibot :This server is rather new\r\n:tmi.twitch.tv 004 wondibot :-\r\n:tmi.twitch.tv 375 wondibot :-\r\n:tmi.twitch.tv 372 wondibot :You are in a maze of twisty passages, all alike.\r\n:tmi.twitch.tv 376 wondibot :>\r\n"

samplePingMessage = "PING :tmi.twitch.tv\r\n"

sampleJoinMessage = ":wondibot!wondibot@wondibot.tmi.twitch.tv JOIN #wondible\r\n"

sampleRoomStateMessage = "@broadcaster-lang=;emote-only=0;followers-only=-1;r9k=0;rituals=0;room-id=56623426;slow=0;subs-only=0 :tmi.twitch.tv ROOMSTATE #wondible\r\n"

sampleNamesMessage = ":wondibot.tmi.twitch.tv 353 wondibot = #wondible :wondibot\r\n:wondibot.tmi.twitch.tv 366 wondibot #wondible :End of /NAMES list\r\n"

sampleChatMessage = ":wondible!wondible@wondible.tmi.twitch.tv PRIVMSG #wondible :test\r\n"

sampleTaggedChatMessage = "@badges=broadcaster/1;color=#1E90FF;display-name=wondible;emotes=;flags=;id=036fe963-8707-44a1-8fb2-e1412343825d;mod=0;room-id=56623426;subscriber=0;tmi-sent-ts=1546013301508;turbo=0;user-id=56623426;user-type= :wondible!wondible@wondible.tmi.twitch.tv PRIVMSG #wondible :test\r\n"

sampleEmoteChatMessage = "@badges=;color=#1E90FF;display-name=Stay_Hydrated_Bot;emotes=869375:0-11/1:94-95;flags=;id=15992f17-5504-4879-80df-2c81b55b3422;mod=0;room-id=56623426;subscriber=0;tmi-sent-ts=1546015898754;turbo=0;user-id=183484964;user-type= :stay_hydrated_bot!stay_hydrated_bot@stay_hydrated_bot.tmi.twitch.tv PRIVMSG #wondible :stayhyBottle [reminder] Live for 2 hours. Total water consumed should be at least 8oz (240mL) :)\r\n"

sampleEmoteRepeatedChatMessage = "@badges=global_mod/1,turbo/1;color=#0D4200;display-name=dallas;emotes=25:0-4,12-16/1902:6-10;id=b34ccfc7-4977-403a-8a94-33c6bac34fb8;mod=0;room-id=1337;subscriber=0;tmi-sent-ts=1507246572675;turbo=1;user-id=1337;user-type=global_mod :ronni!ronni@ronni.tmi.twitch.tv PRIVMSG #dallas :Kappa Keepo Kappa\r\n"

sampleBitsChatMessage = "@badges=staff/1,bits/1000;bits=100;color=;display-name=dallas;emotes=;id=b34ccfc7-4977-403a-8a94-33c6bac34fb8;mod=0;room-id=1337;subscriber=0;tmi-sent-ts=1507246572675;turbo=1;user-id=1337;user-type=staff :ronni!ronni@ronni.tmi.twitch.tv PRIVMSG #dallas :cheer100\r\n"

sampleResubMessage = "@badges=staff/1,broadcaster/1,turbo/1;color=#008000;display-name=ronni;emotes=;id=db25007f-7a18-43eb-9379-80131e44d633;login=ronni;mod=0;msg-id=resub;msg-param-months=6;msg-param-sub-plan=Prime;msg-param-sub-plan-name=Prime;room-id=1337;subscriber=1;system-msg=ronni\\shas\\ssubscribed\\sfor\\s6\\smonths!;tmi-sent-ts=1507246572675;turbo=1;user-id=1337;user-type=staff :tmi.twitch.tv USERNOTICE #dallas :Great stream -- keep it up!\r\n"

-- edited to fix apparent field typo
sampleGiftedSubMessage = "@badges=staff/1,premium/1;color=#0000FF;display-name=TWW2;emotes=;id=e9176cd8-5e22-4684-ad40-ce53c2561c5e;login=tww2;mod=0;msg-id=subgift;msg-param-months=1;msg-param-recipient-display-name=Mr_Woodchuck;msg-param-recipient-id=89614178;msg-param-recipient-user-name=mr_woodchuck;msg-param-sub-plan-name=House\\sof\\sNyoro~n;msg-param-sub-plan=1000;room-id=19571752;subscriber=0;system-msg=TWW2\\sgifted\\sa\\sTier\\s1\\ssub\\sto\\sMr_Woodchuck!;tmi-sent-ts=1521159445153;turbo=0;user-id=13405587;user-type=staff :tmi.twitch.tv USERNOTICE #forstycup\r\n"

sampleAnonGiftedSubMessage = "@badges=broadcaster/1,subscriber/6;color=;display-name=qa_subs_partner;emotes=;flags=;id=b1818e3c-0005-490f-ad0a-804957ddd760;login=qa_subs_partner;mod=0;msg-id=anonsubgift;msg-param-months=3;msg-param-recipient-display-name=TenureCalculator;msg-param-recipient-id=135054130;msg-param-recipient-user-name=tenurecalculator;msg-param-sub-plan-name=t111;msg-param-sub-plan=1000;room-id=196450059;subscriber=1;system-msg=An\\sanonymous\\suser\\sgifted\\sa\\sTier\\s1\\ssub\\sto\\sTenureCalculator!\\s;tmi-sent-ts=1542063432068;turbo=0;user-id=196450059;user-type= :tmi.twitch.tv USERNOTICE #qa_subs_partner\r\n"

-- removed duplicate timestamp
sampleRaidedMessage = "@badges=;color=;display-name=ShyDies;emotes=;flags=;id=c0b6afe2-66d3-4478-89ce-f1fcfe272588;login=shydies;mod=0;msg-id=raid;msg-param-displayName=ShyDies;msg-param-login=shydies;msg-param-profileImageURL=https://static-cdn.jtvnw.net/jtv_user_pictures/7332f19c-fc2f-4807-9800-1214887d3a3e-profile_image-70x70.jpg;msg-param-viewerCount=21;room-id=56623426;subscriber=0;system-msg=21\\sraiders\\sfrom\\sShyDies\\shave\\sjoined\\n!;tmi-sent-ts=1546702285842;turbo=0;user-id=216773601;user-type= :tmi.twitch.tv USERNOTICE #wondible\r\n"

sampleNewChatterMessage = "@badges=;color=;display-name=SevenTest1;emotes=30259:0-6;id=37feed0f-b9c7-4c3a-b475-21c6c6d21c3d;login=seventest1;mod=0;msg-id=ritual;msg-param-ritual-name=new_chatter;room-id=6316121;subscriber=0;system-msg=Seventoes\\sis\\snew\\shere!;tmi-sent-ts=1508363903826;turbo=0;user-id=131260580;user-type= :tmi.twitch.tv USERNOTICE #seventoes :HeyGuys\r\n"

samplePurgeMessage = "@ban-duration=1;room-id=27847836;target-user-id=44890575;tmi-sent-ts=1546359140305 :tmi.twitch.tv CLEARCHAT #darktwinge :landscapegoat\r\n"

sampleHostNoticeMessage = "@msg-id=host_on :tmi.twitch.tv NOTICE #wondible :Now hosting ZermistTV.\r\n"

sampleHostTargetMessage = ":tmi.twitch.tv HOSTTARGET #wondible :zermisttv 3\r\n"

sampleHostTargetOffMessage = ":tmi.twitch.tv HOSTTARGET #wondible :- 0\r\n"

sampleHostTargetWentOfflineMessasge = "@msg-id=host_target_went_offline :tmi.twitch.tv NOTICE #wondible :gman8d has gone offline. Exiting host mode.\r\n"
