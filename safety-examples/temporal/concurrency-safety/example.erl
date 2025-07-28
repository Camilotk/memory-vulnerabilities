-module(example).
-export([start/0, increment/1, get_count/1]).

start() -> spawn(fun() -> loop(0) end).
increment(Pid) -> Pid ! increment.
get_count(Pid) ->
    Pid ! {get_count, self()},
    receive {Pid, Count} -> Count after 5000 -> timeout end.

loop(Count) ->
    receive
        increment -> loop(Count + 1);
        {get_count, From} ->
            From ! {self(), Count},
            loop(Count)
    end.
