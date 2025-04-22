%%%-------------------------------------------------------------------
%%% @doc
%%% Demonstrate Erlangs protection against Buffer Overflow
%%
%%% Example: 
%%%   validate_password("password").
%%% @end
%%%-------------------------------------------------------------------
-module(login).
-author("Camilo de Azevedo <camilotk@gmail.com>").
-export([validate_password/1]).

%%doc Handles user authentication with a password prompt
validate_password(InputPassword) ->
    case file:read_file("password.txt") of
        {ok, StoredPasswordBinary} ->
            % Convert binary to string and remove trailing newline if present
            StoredPassword = string:trim(binary_to_list(StoredPasswordBinary)),
            
            % In Erlang, strings have no fixed size limit
            % We can safely handle passwords of any length
            io:format("Checking password...~n"),
            
            case InputPassword =:= StoredPassword of
                true -> 
                    io:format("Authentication successful!~n"),
                    true;
                false -> 
                    io:format("Authentication failed.~n"),
                    false
            end;
        {error, Reason} ->
            io:format("Error reading password file: ~p~n", [Reason]),
            false
    end.

