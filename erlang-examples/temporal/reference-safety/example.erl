-module(example).
-export([safe_memory/0]).

safe_memory() ->
    % In Erlang, memory is managed by the runtime.
    % Variables are immutable, preventing use-after-free scenarios.

    % This creates a string (list) in memory
    Buffer = "Hello",
    io:format("Buffer content: ~s~n", [Buffer]),

    % Once a variable is no longer referenced,
    % the garbage collector reclaims the memory.
    % There’s no way to explicitly free memory and continue using it.

    % Creating a new variable doesn’t affect the old one
    NewBuffer = "World",
    io:format("New buffer: ~s~n", [NewBuffer]),
    io:format("Original buffer still safe: ~s~n", [Buffer]).
