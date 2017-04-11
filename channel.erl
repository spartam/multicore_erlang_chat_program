-module(channel).

-export([create/4, broadcast/2]).

create(Name, Users, Members, Messages) ->
	channel_loop(Name, Users, Messages, Members).

%% ChannelName is a string
%% Users is a dict {name : PID}
%% Messages is a list
%% Members is a list
channel_loop(ChannelName, Users, Messages, Members) ->
	receive 
		{Sender, history} ->
			Sender ! {self(), channel_history, Messages},
			channel_loop(ChannelName, Users, Messages, Members);

		%% Todo, is sender member of users?
		{Sender, send_message, UserName, MessageText, SendTime} ->
			Message = {message, UserName, ChannelName, MessageText, SendTime},
			UsersWithoutSender = dict:erase(UserName, Users),
			% Size = dict:size(UsersWithoutSender),
			% io:fwrite("Size: ~w~n", [Size]),
			spawn_link(?MODULE, broadcast, [UsersWithoutSender, Message]),
			% broadcast(UsersWithoutSender, Message),
			Sender ! {self(), message_sent},
			channel_loop(ChannelName, Users, Messages ++ [Message], Members);

		{_Sender, login, UserName, PID} ->
			case logged_out_member_check(UserName, Users, Members) of 
				true -> 
					NewUsers = dict:store(UserName, PID, Users),
					Sender ! {self(), logged_in_channel, ChannelName},
					channel_loop(ChannelName, NewUsers, Messages, Members);
				false ->
					channel_loop(ChannelName, Users, Messages, Members)
			end;

		{Sender, register, UserName} ->
			case lists:member(UserName, Members) of 
				true ->
					Sender ! {self(), already_member},
					channel_loop(ChannelName, Users, Messages, Members);
				false ->
					Sender ! {self(), channel_joined, ChannelName},
					NewUsers = dict:store(UserName, Sender, Users),
					channel_loop(ChannelName, NewUsers, Messages, Members ++ [UserName])
			end;

		{Sender, register, UserName, no_login} ->
			case lists:member(UserName, Members) of 
				true ->
					Sender ! {self(), already_member},
					channel_loop(ChannelName, Users, Messages, Members);
				false ->
					Sender ! {self(), channel_joined},
					channel_loop(ChannelName, Users, Messages, Members ++ [UserName])
			end;

		{_Sender, unregister, UserName} ->
			NewUsers = dict:erase(UserName, Users),
			NewMembers = lists:delete(UserName, Members),
			channel_loop(ChannelName, NewUsers, Messages, NewMembers);

		{_Sender, logout, UserName} ->
			NewUsers = dict:erase(UserName, Users),
			channel_loop(ChannelName, NewUsers, Messages, Members);

		{Sender, members} ->
			Sender ! {self(), members, Members},
			channel_loop(ChannelName, Users, Messages, Members);

		{Sender, logged_in} ->
			Logged_in = dict:fetch_keys(Users),
			Sender ! {self(), logged_in, Logged_in},
			channel_loop(ChannelName, Users, Messages, Members);

		Other ->
			io:fwrite("~p", [Other]),
			channel_loop(ChannelName, Users, Messages, Members)
	end.

broadcast(Users, Message) ->
	% StartTime = os:timestamp(),
	dict:map(fun (_, Client) ->
			Client ! {self(), new_message, Message}
		end, Users).
	% Size = dict:size(Users),
	% io:fwrite("Users Size: ~w~n", [Size]),
	% Time = timer:now_diff(os:timestamp(), StartTime),
	% io:format("Channel to user broadcast time = ~p ms~n",
 %    	[Time / 1000.0]).

logged_out_member_check(UserName, Logged_in, Members)->
	case dict:find(UserName, Logged_in) of
		{ok, Value} ->	%% already logged in
			false;
		error ->	%% not logged in
			case lists:member(UserName, Members) of
				true ->
					true;
				false ->
					false
			end
	end.