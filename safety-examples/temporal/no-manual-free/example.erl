-module(example).
-export([allocate_and_use_data/0]).

allocate_and_use_data() ->
    Data = lists:duplicate(100, $a),
    io:format("Data created: ~p~n", [Data]),
    % No 'free' operation. 'Data' becomes eligible for GC
    % when it's no longer reachable after this function returns.
    ok.
