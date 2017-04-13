-module(final_benchmarks).
-export([main/0, wait_for_messages/2]).


initialize_server() ->
	catch unregister(central_server),
	central_server:initialize().

main() ->
	initialize_server(),
	register_user(5),
	Users = login_users(5),
	timer:sleep(10000),
	send_messages(Users).


register_user(NumberOfMember) ->
	io:fwrite("registering peeps~n"),
	statistics(runtime),        % CPU time, summed for all threads
    StartTime = os:timestamp(), % Wall clock time

	ClientsList = lists:map(fun(I) ->
								ClientID = spawn(client, client, [I, central_server, register]),
								ClientID ! {self(), join_channel, "Channel1"},
								receive
									{ClientID, join_successful} ->
										{I, ClientID}
								end,
								ClientID ! {self(), logout}
					 		end, lists:seq(1, NumberOfMember)),

	Clients = dict:from_list(ClientsList),

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

send_messages(Users) ->
	io:fwrite("~npeeps stalking peeps~n"),
	
	statistics(runtime),        % CPU time, summed for all threads
    StartTime = os:timestamp(), % Wall clock time

	dict:map(fun (_I, PID) ->
				% io:fwrite("Send, ~p~n", [I]),
				PID ! {self(), send_message, "Channel1", "Checking in."}
			 end, Users),
	
	Size = dict:size(Users),

	dict:map(fun (_I, PID) ->
				% spawn(?MODULE, wait_for_messages, [self(), PID, Size]) 
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
			io:fwrite("~n~p waiting: ~p/~p~n", [PID, Len, NumberOfMessages]),
			if Len =:= NumberOfMessages
					-> ok;
			true 	->
				wait_for_messages(PID, NumberOfMessages)
			end
	end.





