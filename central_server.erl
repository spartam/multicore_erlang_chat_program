-module(central_server).

% -export([initialize/0, initialize_with/3, central_server/3, typical_session_1/1, typical_session_2/1]).
-export([initialize/0, initialize_with/3, init_central_server/3, typical_session_1/1, typical_session_2/1, broadcast/2]).

-include_lib("eunit/include/eunit.hrl").

%% Code from server_centralized
initialize() ->
    PID = initialize_with(dict:new(), sets:new(), dict:new()),
    PID.

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
			Sender ! {self(), logged_in},
			% spawn_link(?MODULE, broadcast, [Channels, {self(), login, UserName, Sender}]),
			broadcast(Channels, {self(), login, UserName, Sender}),
			server_loop(Users, New_LoggedIn, Channels);

		{Sender, log_out, UserName} ->
			New_LoggedIn = sets:del_element(UserName, LoggedIn),
			% spawn_link(?MODULE, broadcast, [Channels, {self(), logout, UserName}]),
			broadcast(Channels, {self(), logout, UserName}),
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

		{Sender, members, ChannelName} ->
			NewChannels = send_to_channel(ChannelName, Channels, {Sender, members}),
			server_loop(Users, LoggedIn, NewChannels);		

		{Sender, logged_in, ChannelName} ->
			NewChannels = send_to_channel(ChannelName, Channels, {Sender, logged_in}),
			server_loop(Users, LoggedIn, NewChannels);	

		{Sender, channels} ->
			Sender ! {self(), channels, Channels},
			server_loop(Users, LoggedIn, Channels);				

		_Other -> 
			server_loop(Users, LoggedIn, Channels)
	end.

register_user_to_channels(User, Channels, Channels_PID) ->
	lists:foreach(
		fun (Channel) ->
			register_user_to_channel(User, Channel, Channels_PID)
		end, Channels).

register_user_to_channel(User, Channel, Channels) ->
	Channel_Process = dict:fetch(Channel, Channels),
	Channel_Process ! {self(), register, User, no_login}.


broadcast(Channels, Message) ->
	% StartTime = os:timestamp(),
	dict:map(fun (_, PID) -> PID ! Message end, Channels).
	% Time = timer:now_diff(os:timestamp(), StartTime),
	% io:format("Channel broadcast time = ~p ms~n",
 %        [Time / 1000.0]).

send_to_channel(Channel, Channels, Message) ->
	case dict:find(Channel, Channels) of
		{ok, PID} 	-> 	PID ! Message,
						Channels;
		error 		-> 	PID = spawn_link(channel, create, [Channel, dict:new(), [], []]),
						PID ! Message,
						NewChannels = dict:store(Channel, PID, Channels),
						NewChannels
	end.




% test() ->
% 	NumberOfChannels = 100000,
%     NumberOfUsers = 50000,
%     % ChannelNames = ["alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel", "india", "juliet"],
%     ChannelNames = lists:seq(1, NumberOfChannels),
%     UserNames = lists:seq(1, NumberOfUsers),
%     Channels = dict:from_list(lists:map(fun (Name) ->
%         Messages = [{message, 5, Name, "Hello!", os:system_time()},
%                     {message, 6, Name, "Hi!", os:system_time()},
%                     {message, 5, Name, "Bye!", os:system_time()}],
%         Channel = {channel, Name, Messages},
%         {Name, Channel}
%         end,
%         ChannelNames)),
%     Users = dict:from_list(lists:map(fun (Name) ->
%         Subscriptions = [rand:uniform(NumberOfChannels),
%                          rand:uniform(NumberOfChannels),
%                          rand:uniform(NumberOfChannels)],
%         User = {user, Name, sets:from_list(Subscriptions)},
%         {Name, User}
%         end,
%         UserNames)),

%     ServerPid = initialize_with(Users, [], Channels),
%     ServerPid ! {self(), get_channel_history, 3}.
%     % receive
%     %     {_ResponsePid, channel_history, Messages} ->
%     %         io:fwrite("~p~n", [Messages])
%     % end.

initialize_test() ->
	catch unregister(central_server),
	io:fwrite("test~n"),
	initialize().

register_user_test() ->
   	initialize_test(),
    ?assertMatch({_, user_registered}, server:register_user(central_server, "A")),
    ?assertMatch({_, user_registered}, server:register_user(central_server, "B")),
    ?assertMatch({_, user_registered}, server:register_user(central_server, "C")),
    ?assertMatch({_, user_registered}, server:register_user(central_server, "D")),
    ["A", "B", "C", "D"].

log_in_test() ->
    [UserName1, UserName2 | _] = register_user_test(),
    ?assertMatch({_Server1, logged_in}, server:log_in(central_server, UserName1)),
    ?assertMatch({_Server2, logged_in}, server:log_in(central_server, UserName2)).
    % Note: returned pids _Server1 and _Server2 do not necessarily need to be
    % the same.

log_out_test() ->
    [UserName1, UserName2 | _] = register_user_test(),
    {Server1, logged_in} = server:log_in(central_server, UserName1),
    {Server2, logged_in} = server:log_in(central_server, UserName2),
    ?assertMatch(logged_out, server:log_out(Server1, UserName1)),
    ?assertMatch(logged_out, server:log_out(Server2, UserName2)).

join_channel_test() ->
    [UserName1 | _] = register_user_test(),
    io:fwrite("Users registered~n"),
    {Server1, logged_in} = server:log_in(central_server, UserName1),
    io:fwrite("User logged in~n"),
    ?assertMatch(channel_joined,
        server:join_channel(Server1, UserName1, "Channel1")),
    io:fwrite("Joined channel1~n"),
    ?assertMatch(channel_joined,
        server:join_channel(Server1, UserName1, "Channel2")),
    io:fwrite("Joined channel2~n"),
    io:fwrite("get members test~n"),
    ?assertMatch([UserName1],
        server:get_members(Server1, "Channel1")),
    io:fwrite("get logged in test~n"),
    ?assertMatch([UserName1],
        server:get_currently_logged_in(Server1, "Channel1")),
    {UserName1, Server1, "Channel1", "Channel2"}.

send_message_test() ->
    {UserName1, Server1, Channel1, _Channel2} = join_channel_test(),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName1, Channel1, "Hello!")),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName1, Channel1, "How are you?")).

channel_history_test() ->
    % Create users, log in, join channels.
    [UserName1, UserName2 | _] = register_user_test(),
    {Server1, logged_in} = server:log_in(central_server, UserName1),
    {Server2, logged_in} = server:log_in(central_server, UserName2),
    Channel1 = "Channel1",
    server:join_channel(Server1, UserName1, Channel1),
    server:join_channel(Server2, UserName2, Channel1),

    % Send some messages
    server:send_message(Server1, UserName1, Channel1, "Hello!"),
    server:send_message(Server2, UserName2, Channel1, "Hi!"),
    server:send_message(Server1, UserName1, Channel1, "How are you?"),

    % Check history
    [{message, UserName1, Channel1, "Hello!", Time1},
     {message, UserName2, Channel1, "Hi!", Time2},
     {message, UserName1, Channel1, "How are you?", Time3}] =
        server:get_channel_history(Server1, Channel1),
    ?assert(Time1 =< Time2),
    ?assert(Time2 =< Time3).

typical_session_test() ->
    initialize_test(),
    Session1 = spawn_link(?MODULE, typical_session_1, [self()]),
    Session2 = spawn_link(?MODULE, typical_session_2, [self()]),
    receive
        {Session1, ok} ->
            receive
                {Session2, ok} ->
                    done
            end
    end.

typical_session_1(TesterPid) ->
    {_, user_registered} = server:register_user(central_server, "Jennifer"),
    {Server, logged_in} = server:log_in(central_server, "Jennifer"),
    channel_joined = server:join_channel(Server, "Jennifer", "multicore"),
    message_sent = server:send_message(Server, "Jennifer", "multicore", "Hello!"),
    % Wait for reply
    Time2 = receive
        {_, new_message, Message} ->
            ?assertMatch({message, "Janwillem", "multicore", "Hi!", _}, Message),
            {message, _, _, _, Time} = Message,
            Time
    end,
    % Respond
    message_sent = server:send_message(Server, "Jennifer", "multicore", "How are you?"),

    % Check history
    [{message, "Jennifer",  "multicore", "Hello!",       Time1},
     {message, "Janwillem", "multicore", "Hi!",          Time2},
     {message, "Jennifer",  "multicore", "How are you?", Time3}] =
        server:get_channel_history(Server, "multicore"),
    ?assert(Time1 =< Time2),
    ?assert(Time2 =< Time3),

    TesterPid ! {self(), ok}.

typical_session_2(TesterPid) ->
    {_, user_registered} = server:register_user(central_server, "Janwillem"),
    {Server, logged_in} = server:log_in(central_server, "Janwillem"),
    channel_joined = server:join_channel(Server, "Janwillem", "multicore"),
    % Wait for first message
    Time1 = receive
        {_, new_message, Message1} ->
            ?assertMatch({message, "Jennifer", "multicore", "Hello!", _}, Message1),
            {message, _, _, _, Time} = Message1,
            Time
    end,
    % Reply
    message_sent = server:send_message(Server, "Janwillem", "multicore", "Hi!"),
    % Wait for response
    Time3 = receive
        {_, new_message, Message3} ->
            ?assertMatch({message, "Jennifer", "multicore", "How are you?", _}, Message3),
            {message, _, _, _, Time_} = Message3,
            Time_
    end,

    % Check history
    [{message, "Jennifer",  "multicore", "Hello!",       Time1},
     {message, "Janwillem", "multicore", "Hi!",          Time2},
     {message, "Jennifer",  "multicore", "How are you?", Time3}] =
        server:get_channel_history(Server, "multicore"),
    ?assert(Time1 =< Time2),
    ?assert(Time2 =< Time3),

    TesterPid ! {self(), ok}.
