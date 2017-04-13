-module(tests).

-export([initialize_test/0, register_clients_test/0, register_clients_to_channels/0, channels_test/0, messages_tests/0]).
-include_lib("eunit/include/eunit.hrl").

initialize_test() ->
	catch unregister(central_server),
	central_server:initialize().

register_clients_test() ->
	io:fwrite("Register clients~n"),
	initialize_test(),
	A = spawn_link(client, client, ["Spartam", central_server, register]),
	B = spawn_link(client, client, ["Criek", central_server, register]),
	C = spawn_link(client, client, ["ytreza", central_server, register]),
	[A, B, C].

register_clients_to_channels() ->
	[A, B, C] = register_clients_test(),
	io:fwrite("register clients to channels~n"),
	A ! {self(), join_channel, "Channel1"},
	receive
		{A, join_successful} ->
			io:fwrite("A join successful~n"),
			ok
	end,
	B ! {self(), join_channel, "Channel1"},
	B ! {self(), join_channel, "Channel2"},
	receive
		{B, join_successful} ->
			io:fwrite("B join successful 1~n"),
			ok
	end,
	receive
		{B, join_successful} ->
			io:fwrite("B join successful 2~n"),
			ok
	end,
	C ! {self(), join_channel, "Channel2"},
	receive
		{C, join_successful} ->
			io:fwrite("C join successful~n"),
			ok
	end,
	[A, B, C].


channels_test() ->
	[A, B, C] = register_clients_to_channels(),
	central_server ! {self(), channels},

	receive
		{_Central, channels, Channels1} ->
			io:fwrite("channels ~p~n", [Channels1]),
			?assert(dict:size(Channels1) =:= 2),
			Keys = dict:fetch_keys(Channels1),
			?assert(lists:member("Channel1", Keys)),
			?assert(lists:member("Channel2", Keys))
	end,

	io:fwrite("Check channels~n"),
	
	A ! {self(), channels},

	receive
		{A, channels, Channels2} ->
			io:fwrite("A: ~p~n", [Channels2]),
			?assert(length(Channels2) =:= 1),
			?assert(lists:member("Channel1", Channels2))
	end,

	B ! {self(), channels},

	receive
		{B, channels, Channels3} ->
			io:fwrite("B: ~p~n", [Channels3]),
			?assert(length(Channels3) =:= 2),
			?assert(lists:member("Channel1", Channels3)),
			?assert(lists:member("Channel2", Channels3))
	end,

	C ! {self(), channels},

	receive
		{C, channels, Channels4} ->
			io:fwrite("C: ~p~n", [Channels4]),
			?assert(length(Channels4) =:= 1),
			?assert(lists:member("Channel2", Channels4))
	end,
	[Channels1, A, B, C].

messages_tests() ->
	[Channels, A, B, C] = channels_test(),
	A ! {self(), send_message, "Channel1", "Hello Everyone"},
	C ! {self(), send_message, "Channel2", "Hello Everyone"},
	A ! {self(), send_message, "Channel1", "How is everyone doing?"},
	C ! {self(), send_message, "Channel2", "How is everyone doing?"},
	B ! {self(), send_message, "Channel1", "Hello, I'm fine. Thanks for asking"},
	B ! {self(), send_message, "Channel2", "Hello, I'm fine. Thanks for asking"},

	A ! {self(), history},
	receive
		{A, history, MessagesA} ->
			io:fwrite("A Messages: ~p~n~n", [MessagesA])
	end,

	B ! {self(), history},
	receive
		{B, history, MessagesB} ->
			io:fwrite("B Messages: ~p~n~n", [MessagesB])
	end.
