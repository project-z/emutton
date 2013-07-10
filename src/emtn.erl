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
    io:format("loop.... ~n~n"),
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, term_to_binary(Msg)}},
            receive
                {Port, {data, Data}} ->
                    io:format("Data received: ~p ~n~n", [binary_to_term(Data)]),
                    Caller ! {emtn, binary_to_term(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), {command, halt_emtn()}},
            receive
                {'EXIT', P2, Reason} ->
                    io:format("Port: ~p replied, reason: ~p... ~n", [P2, Reason]),
                    erlang:unlink(P2),
                    erlang:port_close(P2),
                    exit(normal);
                Msg ->
                    io:format("Response: ~p", [Msg]),
                    exit(error)
            end;
        {'EXIT', P3, Reason} ->
            io:format("Port: ~p stopped; reason: ~p ~n", [P3, Reason]),
            exit(port_terminated)
    end.

halt_emtn() -> {done, 86}.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, {emtn, P}} = start(),
    Msg = stop(). % this calls port_close/1 which returns true

-endif.
