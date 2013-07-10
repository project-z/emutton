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
-define(APP_NAME, emutton).
-define(EXT_PROG, "emtn_prog").


start() ->
    spawn(?MODULE, init, [?EXT_PROG]),
    ok.


stop() ->
    %emtn ! stop.
    call_port(halt_emtn()).


% TODO - find code:priv_dir, then append ?EXT_PROG - this doesn't work from tests.
init(ExtProgName) ->
    register(emtn, self()),
    process_flag(trap_exit, true),
    {Port, PrivDir} = create_port(ExtProgName),

    filelib:ensure_dir("tmp/demo/"), % <= make sure to have your trailing slash
    case filelib:is_file("lua_scripts") of
        true -> ok;
        false ->
            filelib:ensure_dir("./lua_scripts/"), % <= BAM! Trailing SLASH!
            LuaScriptDir = filename:join([PrivDir, "lua_scripts/"]),
            LuaScripts = filelib:fold_files(LuaScriptDir, "*.lua", true,
                                            fun(F,Acc)->
                                                io:format("~p~n", [F]),
                                                [F|Acc]
                                            end, []),
            io:format("~p~n", LuaScripts)
    end,

    loop(Port).


create_port(ExtProgName) ->
    case code:priv_dir(?APP_NAME) of
        {error, _} ->
            error_logger:format("~w priv directory not found.~n", [?APP_NAME]),
            exit(error);
        PrivDir ->
            Port = erlang:open_port({spawn, filename:join([PrivDir, ExtProgName])},
                [{packet, 2}, binary, exit_status]),
            {Port, PrivDir}
    end.


call_port(Msg) ->
    emtn ! {call, self(), Msg},
    receive
        {emtn, Result} ->
            Result;
        Msg ->
            Msg
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
    %% this sucks, but stop/0 can be called before start/0 is complete and the
    %% test fails... this is a brittle thing, but it seems common in erlang
    %% unit testing according to the author of Learn You Some Erlang:
    %%
    %%      see http://learnyousomeerlang.com/eunit#testing-regis - then scroll
    %%      to the "Don't Drink Too Much Kool-Aid" sidebar
    timer:sleep(17),
    650 = stop().
-endif.
