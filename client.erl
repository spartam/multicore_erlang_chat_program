-module(client).

-export([client/3, client_run/0]).

client(UserName, Server, register) ->
	% io:fwrite("make client: ~p~n", [UserName]),
	Server ! {self(), register_user, UserName},
	receive
		{_S, user_registered} ->
			% io:fwrite("registered: ~p~n", [UserName]),
			clientloop(UserName, dict:new(), Server, [])
	end;
client(UserName, Server, login) ->
	Server ! {self(), log_in, UserName},
	receive
		{Server, logged_in} ->
		clientloop(UserName, dict:new(), Server, [])
	end.


clientloop(UserName, Channels, Server, Messages) ->
	receive
		{ChannelServer, logged_in_channel, Name} ->
			NewChannels = join_channel(Name, ChannelServer, Channels),
			clientloop(UserName, NewChannels, Server, Messages);

		{_Client, send_message, ChannelName, MessageText} ->
			SendTime = os:system_time(),
			Message = {message, UserName, ChannelName, MessageText, SendTime},
			ChannelMsg = {self(), send_message, UserName, MessageText, SendTime},
			send_message(ChannelName, Channels, ChannelMsg),
			clientloop(UserName, Channels, Server, Messages ++ [Message]);

		% {<0.40.0>,new_message, {message,"ytreza","Channel2","How is everyone doing?", 1491966910295328038}}

		{_ChannelID, new_message, {message, User, ChannelName, MessageText, SendTime}} ->
			io:fwrite("~p - [~p]~p: ~p~n", [SendTime, ChannelName, User, MessageText]),
			clientloop(UserName, Channels, Server, Messages ++ [{message, User, ChannelName, MessageText, SendTime}]);

		{ChannelID, channel_joined, ChannelName} ->
			NewChannels = join_channel(ChannelName, ChannelID, Channels),
			clientloop(UserName, NewChannels, Server, Messages);

		{Client, join_channel, ChannelName} ->
			% io:fwrite("join_channel~n"),
			Server ! {self(), join_channel, UserName, ChannelName},
			Client ! {self(), join_successful},
			clientloop(UserName, Channels, Server, Messages);

		{Client, channels} ->
			ChannelNames = dict:fetch_keys(Channels),
			Client ! {self(), channels, ChannelNames},
			clientloop(UserName, Channels, Server, Messages);

		{Client, history} ->
			Client ! {self(), history, Messages},
			clientloop(UserName, Channels, Server, Messages);

		Other ->
			% io:fwrite("Client Other: ~p~n", [Other]),
			clientloop(UserName, Channels, Server, Messages)
	end.



join_channel(Name, ChannelID, Channels) ->
	case dict:find(Name, Channels) of
		{ok, _Value} ->
			Channels;
		error ->
			NewChannels = dict:store(Name, ChannelID, Channels),
			NewChannels
	end.

send_message(ChannelName, Channels, Message) ->
	case dict:find(ChannelName, Channels) of
		{ok, ChannelID} ->
			% io:fwrite("send message ~p~p : ~p~n", [ChannelName, ChannelID, Message]),
			ChannelID ! Message,
			true;
		error ->
			% io:fwrite("You didn't actually believe this would work, did you?~n"),
			false
	end.


client_run() ->
	{ok, [Value]} = io:fread("username > ", "~s").