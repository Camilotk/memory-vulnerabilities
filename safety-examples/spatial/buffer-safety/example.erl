-module(example).
-export([handle_string/1]).

handle_string(InputString) when is_list(InputString) ->
    io:format("Received string: ~ts~n", [InputString]);
handle_string(InputBin) when is_binary(InputBin) ->
    io:format("Received binary string: ~s~n", [InputBin]).
