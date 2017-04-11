-module(client).

-export([client/1]).

client(UserName) ->
	clientloop(UserName, dict:new()).


clientloop(UserName, Channels) ->
	receive
		{ChannelServer, channel_joined, Name} ->
			NewChannels = join_channel(Name, ChannelServer, Channels);

		{ServerPid, new_message, {message, UserName, ChannelName, MessageText, SendTime}} ->
			{message, UserName, ChannelName, MessageText, SendTime}
	end.



join_channel(Name, ChannelID, Channels) ->
	case dict:find(Name, Channels) of
		{ok, _Value} ->
			Channels;
		error ->
			NewChannels = dict:store(Name, ChannelID, Channels),
			NewChannels
	end.