-module(central_server).

% -export([initialize/0, initialize_with/3, central_server/3, typical_session_1/1, typical_session_2/1]).
-export([initialize/0, initialize_with/3, test/0, init_central_server/3]).

%% Code from server_centralized
initialize() ->
    initialize_with(dict:new(), sets:new(), dict:new()).

initialize_with(Users, LoggedIn, Channels) ->
    ServerPid = spawn_link(?MODULE, init_central_server, [Users, LoggedIn, Channels]),
    catch unregister(central_server),
    register(central_server, ServerPid),
    ServerPid.

%% Convertion step from benchmark (given code) to own code
%% Channels is a dict channelname : PID with PID the process ID of the channel process
%% Benchmarks gives a dict with name and message history,
%% Users is a dict with name and subscriptions to be converted to a list with usernames
%% Subcriptions are controlled by the channels
init_central_server(Users, LoggedIn, Channels) ->
	Channels_PID = dict:map(fun (_, {channel, Name, Messages}) -> 
								PID = spawn_link(channel, create, [Name, dict:new(), [], Messages]),
								PID
								end, 
					Channels),
	dict:map(
		fun (_, {user, Name, Subscriptions}) ->
			Subs = sets:to_list(Subscriptions),
			register_user_to_channels(Name, Subs, Channels_PID)
		end, Users),
	UsersSet = sets:from_list(dict:fetch_keys(Users)),
	server_loop(UsersSet, LoggedIn, Channels_PID).
	% Channels_PID.


server_loop(Users, LoggedIn, Channels) ->
	receive
		{Sender, register_user, UserName} ->
			New_Users = sets:add_element(UserName, Users),
			New_LoggedIn = sets:add_element(UserName, LoggedIn), %% new users are logged in
			Sender ! {self(), user_registered},
			server_loop(New_Users, New_LoggedIn, Channels);

		{Sender, log_in, UserName} ->
			New_LoggedIn = sets:add_element(UserName, LoggedIn),
			broadcast(Channels, {self(), login, UserName, Sender}),
			Sender ! {self(), logged_in},
			server_loop(Users, New_LoggedIn, Channels);

		{Sender, log_out, UserName} ->
			New_LoggedIn = sets:de_element(UserName, LoggedIn),
			broadcast(Channels, {self(), logout, UserName, Sender}),
			Sender ! {self(), logged_out},
			server_loop(Users, New_LoggedIn, Channels);

		{Sender, join_channel, UserName, ChannelName} ->
			NewChannels = send_to_channel(ChannelName, Channels, {Sender, register, UserName}), %% Relay to channel
			server_loop(Users, LoggedIn, NewChannels);

		{Sender, send_message, UserName, ChannelName, MessageText, SendTime} ->
			NewChannels = send_to_channel(ChannelName, Channels, {Sender, send_message, UserName, MessageText, SendTime}), %% relay to channel
			server_loop(Users, LoggedIn, NewChannels);

		{Sender, get_channel_history, ChannelName} ->
			NewChannels = send_to_channel(ChannelName, Channels, {Sender, history}),
			server_loop(Users, LoggedIn, NewChannels);

		Other -> 
			server_loop(Users, LoggedIn, Channels)
	end.

register_user_to_channels(User, Channels, Channels_PID) ->
	lists:foreach(
		fun (Channel) ->
			register_user_to_channel(User, Channel, Channels_PID)
		end, Channels).

register_user_to_channel(User, Channel, Channels) ->
	Channel_Process = dict:fetch(Channel, Channels),
	Channel_Process ! {self(), register, User}.


broadcast(Channels, Message) ->
	dict:map(fun (_, PID) -> PID ! Message end, Channels).

send_to_channel(Channel, Channels, Message) ->
	case dict:find(Channel, Channels) of
		{ok, PID} 	-> 	PID ! Message,
						Channels;
		error 		-> 	PID = spawn_link(channel, create, [Channel, dict:new(), [], []]),
						PID ! Message,
						NewChannels = dict:store(Channel, PID, Channels),
						NewChannels
	end.




test() ->
	NumberOfChannels = 100000,
    NumberOfUsers = 50000,
    % ChannelNames = ["alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel", "india", "juliet"],
    ChannelNames = lists:seq(1, NumberOfChannels),
    UserNames = lists:seq(1, NumberOfUsers),
    Channels = dict:from_list(lists:map(fun (Name) ->
        Messages = [{message, 5, Name, "Hello!", os:system_time()},
                    {message, 6, Name, "Hi!", os:system_time()},
                    {message, 5, Name, "Bye!", os:system_time()}],
        Channel = {channel, Name, Messages},
        {Name, Channel}
        end,
        ChannelNames)),
    Users = dict:from_list(lists:map(fun (Name) ->
        Subscriptions = [rand:uniform(NumberOfChannels),
                         rand:uniform(NumberOfChannels),
                         rand:uniform(NumberOfChannels)],
        User = {user, Name, sets:from_list(Subscriptions)},
        {Name, User}
        end,
        UserNames)),

    ServerPid = initialize_with(Users, [], Channels),
    ServerPid ! {self(), get_channel_history, 3}.
    % receive
    %     {_ResponsePid, channel_history, Messages} ->
    %         io:fwrite("~p~n", [Messages])
    % end.
