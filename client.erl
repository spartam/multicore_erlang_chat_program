-module(client).

-export([client/1]).

client(UserName, Server, register) ->
	Server ! {self(), register_user, UserName},
	receive
		{Server, user_registered} ->
			clientloop(UserName, dict:new(), Server)
	end.

client(UserName, Server, login) ->
	Server ! {self(), log_in, UserName},
	receive
		{Server, logged_in} ->
		clientloop(UserName, dict:new(), Server)
	end.


clientloop(UserName, Channels, Server) ->
	receive
		{ChannelServer, logged_in_channel, Name} ->
			NewChannels = join_channel(Name, ChannelServer, Channels),
			clientloop(UserName, NewChannels, Server);

		{ChannelID, new_message, {message, UserName, ChannelName, MessageText, SendTime}} ->
			io:fwrite("~p - [~p]~p: ~p", [SendTime, ChannelName, UserName, MessageText]),
			clientloop(UserName, NewChannels, Server);

		{ChannelID, channel_joined, ChannelName} ->
			NewChannels = join_channel(ChannelName, ChannelID, Channels),
			clientloop(UserName, NewChannels, Server);

		{_Client, join_channel, ChannelName} ->
			Server ! {self(), join_channel, UserName, ChannelName},
			clientloop(UserName, NewChannels, Server);

	end.



join_channel(Name, ChannelID, Channels) ->
	case dict:find(Name, Channels) of
		{ok, _Value} ->
			Channels;
		error ->
			NewChannels = dict:store(Name, ChannelID, Channels),
			NewChannels
	end.