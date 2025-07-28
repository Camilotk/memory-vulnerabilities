-module(example).
-export([access_elements/0]).

access_elements() ->
    MyList = [1, 2, 3, 4, 5],
    MyTuple = {a, b, c},

    % Safe access
    io:format("List element 3: ~p~n", [lists:nth(3, MyList)]),
    io:format("Tuple element 1: ~p~n", [element(1, MyTuple)]),

    % Attempted out-of-bounds access for list
    try lists:nth(10, MyList) of
        Val -> io:format("List OOB value: ~p~n", [Val])
    catch
        error:Reason -> io:format("Error accessing list: ~p~n", [Reason])
    end,

    % Attempted out-of-bounds access for tuple
    try element(5, MyTuple) of
        Val -> io:format("Tuple OOB value: ~p~n", [Val])
    catch
        error:Reason -> io:format("Error accessing tuple: ~p~n", [Reason])
    end.
