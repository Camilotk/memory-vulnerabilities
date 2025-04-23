-module(example).
-export([handle_string/1]).

handle_string(InputString) ->
    Buffer = InputString,
    io:format("Buffer content: ~s~n", [Buffer]).
