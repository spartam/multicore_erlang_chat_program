-module(tests).

-export([initialize_test/0, register_clients_test/0, register_clients_to_channels/0]).
-include_lib("eunit/include/eunit.hrl").

initialize_test() ->
	catch unregister(central_server),
	central_server:initialize().

register_clients_test() ->
	initialize_test(),
	A = spawn_link(client, client, [1, central_server, register]),
	B = spawn_link(client, client, [2, central_server, register]),
	C = spawn_link(client, client, [3, central_server, register]),
	[A, B, C].

register_clients_to_channels() ->
	[A, B, C] = register_clients_test(),
	A ! {self(), join_channel, "Channel1"},
	receive
		{A, join_successful} ->
			ok
	end,
	B ! {self(), join_channel, "Channel1"},
	B ! {self(), join_channel, "Channel2"},
	receive
		{B, join_successful} ->
			ok
	end,
	receive
		{B, join_successful} ->
			ok
	end,
	C ! {self(), join_channel, "Channel2"},
	receive
		{C, join_successful} ->
			ok
	end.