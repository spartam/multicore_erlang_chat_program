-module(final_benchmarks).
-export([main/0, wait_for_messages/2]).


initialize_server() ->
	catch unregister(central_server),
	central_server:initialize().

main() ->
	initialize_server(),
	register_user(5000, 50),
	Users = login_users(1000),
	timer:sleep(2500),
	send_messages(Users, 50, 50),
	io:fwrite("~n--~n").


register_user(NumberOfMember, NumberOfChannels) ->
	io:fwrite("registering peeps~n"),
	statistics(runtime),        % CPU time, summed for all threads
    StartTime = os:timestamp(), % Wall clock time

	ClientsList = lists:map(fun(I) ->
								ClientID = spawn(client, client, [I, central_server, register]),
								ClientID ! {self(), join_channel, I rem NumberOfChannels},
								receive
									{ClientID, join_successful} ->
										{I, ClientID}
								end
					 		end, lists:seq(1, NumberOfMember)),

	Clients = dict:from_list(ClientsList),

	dict:map(fun (_I, ClientID) ->
				ClientID ! {self(), logout},
				receive
					{_S1, logged_out} ->
						ok
				end
			end, Clients),

	{_, Time1} = statistics(runtime),
    Time2 = timer:now_diff(os:timestamp(), StartTime),
    io:format("CPU time = ~p ms~nWall clock time = ~p ms~n",
        [Time1, Time2 / 1000.0]),
    Clients.

login_users(NumberOfUsers) ->
	io:fwrite("~nloging in peeps~n"),
	statistics(runtime),        % CPU time, summed for all threads
    StartTime = os:timestamp(), % Wall clock time

	UsersList = lists:map(fun(I) -> 
							ClientID = spawn(client, client, [I, central_server, login]),
							{I, ClientID}
						  end, lists:seq(1, NumberOfUsers)
					 	),
	Users = dict:from_list(UsersList),

	{_, Time1} = statistics(runtime),
    Time2 = timer:now_diff(os:timestamp(), StartTime),
    io:format("CPU time = ~p ms~nWall clock time = ~p ms~n",
        [Time1, Time2 / 1000.0]),
    Users.

send_messages(Users, NumberOfChannels, MessagesPerUser) ->
	io:fwrite("~npeeps stalking peeps~n"),
	
	statistics(runtime),        % CPU time, summed for all threads
    StartTime = os:timestamp(), % Wall clock time

	dict:map(fun (I, PID) ->
				% io:fwrite("Send, ~p~n", [I]),
				lists:foreach(fun (_J) ->
					PID ! {self(), send_message, I rem NumberOfChannels, "Checking in."}
				end, lists:seq(1, MessagesPerUser))
			 end, Users),
	
	% Size = dict:size(Users),

	dict:map(fun (_I, PID) ->
				% spawn(?MODULE, wait_for_messages, [self(), PID, Size])
				PID ! {self(), channels},
				receive
					{_S1, channels, [Channel]} ->
						ok
				end,
				central_server ! {self(), channels},
				receive
					{_S2, channels, ChannelsDict} ->
						ok
				end,
				{ok, ChannelID} = dict:find(Channel, ChannelsDict),
				ChannelID ! {self(), logged_in},
				receive
					{_S3, logged_in, Logged_in} ->
						ok
				end,
				Size = length(Logged_in) * MessagesPerUser,
				wait_for_messages(PID, Size)
			 end, Users),

	% wait_for_ok(Size),

	{_, Time1} = statistics(runtime),
    Time2 = timer:now_diff(os:timestamp(), StartTime),
    io:format("CPU time = ~p ms~nWall clock time = ~p ms~n",
        [Time1, Time2 / 1000.0]).


% wait_for_ok(0) -> ok;
% wait_for_ok(NumberOfOk) ->
% 	io:fwrite("wait_for_ok: ~p~n", [NumberOfOk]),
% 	receive
% 		ok ->
% 			wait_for_ok(NumberOfOk - 1)
% 	end.


wait_for_messages(PID, NumberOfMessages) ->
	% timer:sleep(10000),
	PID ! {self(), history},
	receive
		{_Serv, history, Messages} ->
			Len = length(Messages),
			% io:fwrite("~n~p waiting: ~p/~p~n", [PID, Len, NumberOfMessages]),
			if Len >= NumberOfMessages
					-> ok;
			true 	->
				wait_for_messages(PID, NumberOfMessages)
			end
	end.





