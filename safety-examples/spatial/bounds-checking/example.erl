-module(example).
-export([access_elements/0]).

access_elements() ->
    List = [1, 2, 3, 4, 5],
    Tuple = {a, b, c},

    % Safe access
    io:format("List element 3: ~p~n", [lists:nth(3, List)]),
    io:format("Tuple element 1: ~p~n", [element(1, Tuple)]),

    % List access with error handling
    try lists:nth(10, List) of
        ListVal -> io:format("List value: ~p~n", [ListVal])
    catch
        error:ListError -> io:format("List access error: ~p~n", [ListError])
    end,

    % Tuple access with error handling
    try element(3, Tuple) of
        TupleVal -> io:format("Tuple value: ~p~n", [TupleVal])
    catch
        error:TupleError -> io:format("Tuple access error: ~p~n", [TupleError])
    end.
