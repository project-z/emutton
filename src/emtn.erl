-module(emtn).

-export([
        start/0,
        stop/0,
        init/1
        ]).

-export([
        index/3,
        status/1,
        ping/0
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SHUTDOWN, 86).
-define(EXT_PROG, "./priv/emtn_prog").


start() ->
    spawn(?MODULE, init, [?EXT_PROG]),
    ok.


stop() ->
    %emtn ! stop.
    call_port(halt_emtn()).

% TODO - find code:priv_dir, then append ?EXT_PROG - this doesn't work from tests.
init(ExtProg) ->
    register(emtn, self()),
    process_flag(trap_exit, true),
    Port = erlang:open_port({spawn, ExtProg}, [{packet, 2}, binary, exit_status]),
    loop(Port).


call_port(Msg) ->
    emtn ! {call, self(), Msg},
    receive
        {emtn, Result} ->
            Result
    end.


index(BucketName, EventName, EventPayload) ->
    call_port({index,
                {{bucket, BucketName},
                 {event, EventName},
                 {payload, EventPayload}}}).


status(Int) ->
    call_port({status, Int}).

ping() ->
    call_port({ping}).

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, term_to_binary(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {emtn, binary_to_term(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), {command, halt_emtn()}},
            receive
                {'EXIT', P2, Reason} ->
                    erlang:unlink(P2),
                    erlang:port_close(P2),
                    exit(Reason);
                _Msg ->
                    exit(error)
            end;
        {'EXIT', _P3, _Reason} ->
            exit(port_terminated)
    end.

halt_emtn() -> {done, 86}.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    ok = start(),
    650 = stop(). % this calls port_close/1 which returns true

-endif.
