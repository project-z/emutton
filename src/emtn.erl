-module(emtn).

-export([
         start/0,
         stop/0,
         init/1
        ]).

-export([
        index/1,
        status/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SHUTDOWN, 86).
-define(EXT_PROG, "./priv/emtn_prog").


start() ->
    io:format("WTF!?!?!~n"),
    spawn(?MODULE, init, [?EXT_PROG]).


stop() ->
    emtn ! stop.


init(ExtProg) ->
    register(emtn, self()),
    process_flag(trap_exit, true),
    Port = erlang:open_port({spawn, ExtProg}, [{packet, 2}]),
    io:format("Port? ~p", [Port]),
    loop(Port).


call_port(Msg) ->
    emtn ! {call, self(), Msg},
    receive
        {emtn, Result} ->
            Result
    end.


index(Pkg) ->
    call_port({index, Pkg}).


status(Int) ->
    call_port({status, Int}).


loop(Port) ->
    io:format("loop...."),
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, emtn_encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    io:format("Data received: ~p", Data),
                    Caller ! {emtn, emtn_decode(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), {command, halt_emtn(650)}},
            receive
                {Port, 650} ->
                    io:format("Port: ~p sent back 650...", [Port]),
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            io:format("Port: ~p stopped; reason: ~p", [Port, Reason]),
            exit(port_terminated)
    end.


emtn_encode({index, X}) -> [1, X];
emtn_encode({status, Y}) -> [2, Y].

emtn_decode([Int]) -> Int.

halt_emtn(Code) -> [86, Code].

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, {emtn, P}} = start(),
    Msg = stop(). % this calls port_close/1 which returns true

-endif.
