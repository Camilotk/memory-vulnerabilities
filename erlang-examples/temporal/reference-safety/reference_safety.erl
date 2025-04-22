%%%-------------------------------------------------------------------
%%% @doc
%%% Secure user management system with authentication and admin controls.
%%% @end
%%%-------------------------------------------------------------------
-module(reference_safety).
-author("Camilo de Azevedo <camilotk@gmail.com>").
-export([start/0, init/0]).

-record(user, {username, password, is_admin = false}).

%% @doc Initializes the system with a default admin user.
init() ->
    Users = [#user{username = "admin", password = "admin123", is_admin = true}],
    login_loop(Users, undefined).

%% @doc Main loop handling user commands.
%% Continuously prompts the user for input.
login_loop(Users, CurrentUser) ->
    {Command, Args} = read_command(),
    process_command(Command, Args, Users, CurrentUser).

%% @doc Processes user commands such as login, logout, user addition, admin actions, and exit.
process_command(login, [Username, Password], Users, _) ->
    case authenticate(Users, Username, Password) of
        {ok, User} -> 
            io:format("[+] Logged in as '~s' (admin=~p)~n", [Username, User#user.is_admin]),
            login_loop(Users, User);
        error -> 
            io:format("[-] Login failed: invalid credentials~n"),
            login_loop(Users, undefined)
    end;

process_command(logout, [], Users, CurrentUser) ->
    case CurrentUser of
        undefined -> io:format("[!] No user is logged in~n");
        _ -> io:format("[*] Logging out '~s'~n", [CurrentUser#user.username])
    end,
    login_loop(Users, undefined);

process_command(adduser, [Username, Password, IsAdmin], Users, CurrentUser) ->
    case is_admin(CurrentUser) of
        true -> 
            User = #user{username = Username, password = Password, is_admin = IsAdmin},
            io:format("[+] Added user '~s' (admin=~p)~n", [Username, IsAdmin]),
            login_loop([User | Users], CurrentUser);
        false -> 
            io:format("[-] Only admins can add users~n"),
            login_loop(Users, CurrentUser)
    end;

process_command(admin, [], Users, CurrentUser) ->
    case is_admin(CurrentUser) of
        true -> io:format("[+] Admin action performed by '~s'~n", [CurrentUser#user.username]);
        false -> io:format("[-] Access denied: admin only~n")
    end,
    login_loop(Users, CurrentUser);

process_command(exit, [], _, _) ->
    io:format("Exiting...~n");

process_command(_, _, Users, CurrentUser) ->
    io:format("[!] Unknown command~n"),
    login_loop(Users, CurrentUser).

%% @doc Authenticates a user based on username and password.
authenticate(Users, Username, Password) ->
    case lists:keyfind(Username, #user.username, Users) of
        #user{password = Password} = User -> {ok, User};
        _ -> error
    end.

%% @doc Checks if the current user is an admin.
is_admin(undefined) -> false;
is_admin(#user{is_admin = false}) -> false;
is_admin(#user{is_admin = true}) -> true.

%% @doc Reads a user command from input.
read_command() ->
    {ok, [CommandStr]} = io:fread("> ", "~s"),
    parse_command(CommandStr).

%% @doc Parses the user command input.
parse_command("login") ->
    {ok, [Username, Password]} = io:fread("", "~s ~s"),
    {login, [Username, Password]};
parse_command("logout") -> {logout, []};
parse_command("adduser") ->
    {ok, [Username, Password, IsAdminStr]} = io:fread("", "~s ~s ~s"),
    {adduser, [Username, Password, IsAdminStr == "1"]};
parse_command("admin") -> {admin, []};
parse_command("exit") -> {exit, []};
parse_command(_) -> {unknown, []}.

%% @doc Starts the application.
start() -> init().
