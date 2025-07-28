-module(example).
-export([start/0, request_action/1, release/1]).

% Starts a new process running the loop
start() ->
    spawn(fun() -> loop() end).

% Sends a perform_action request to the process and waits for a reply (up to 5 seconds)
request_action(Pid) ->
    Pid ! {self(), perform_action},
    receive
        {Pid, Result} -> Result
    after 5000 ->
        timeout
    end.

% Sends a terminate message to the process
release(Pid) ->
    Pid ! terminate.

% Main receive loop for handling messages
loop() ->
    receive
        % Handle perform_action request and reply to sender
        {From, perform_action} ->
            From ! {self(), {ok, action_performed}},
            loop();
        % Exit on terminate message
        terminate ->
            ok
    end.
