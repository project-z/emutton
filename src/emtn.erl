-module(emtn).

-export([
        start/0,
        stop/1
        ]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

load_driver() ->
    Dir = filename:join([filename:dirname(code:which(emtn)), "..", "priv"]),
    erl_ddll:load(Dir, "emtn_drv").

start() ->
    case load_driver() of
            ok ->
                Port = open_port({spawn, 'emtn_drv'}, [binary]),
                {ok, {emtn, Port}};
            {error, Err} ->
                Msg = erl_ddll:format_error(Err),
                {error, Msg};
            _ -> exit({error, could_not_load_driver})
    end.

stop({emtn, Port}) ->
    unlink(Port),
    port_close(Port).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, {emtn, P}} = start(),
    true = stop({emtn, P}). % this calls port_close/1 which returns true

-endif.
