-module(emtn).

-export([
        start/0,
        stop/0,
        init/1
        ]).

-export([
        index/3,
        status/1,
        ping/0,
        cwd/0
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
    {Port, _PrivDir} = create_port(ExtProgName),

    loop(Port).


create_port(ExtProgName) ->
    case code:priv_dir(?APP_NAME) of
        {error, _} ->
            error_logger:format("~w priv directory not found.~n", [?APP_NAME]),
            exit(error);
        PrivDir ->
            Cmd = lists:flatten(io_lib:format("~s ~s",
                        [filename:join([PrivDir, ExtProgName]), PrivDir])),
            Port = erlang:open_port({spawn, Cmd}, [{packet, 2},
                                                    binary, exit_status]),
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


cwd() ->
    call_port({cwd}).


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

basic_start_stop_test() ->
    ok = start(),
    %% this sucks, but stop/0 can be called before start/0 is complete and the
    %% test fails... this is a brittle thing, but it seems common in erlang
    %% unit testing according to the author of Learn You Some Erlang:
    %%
    %%      see http://learnyousomeerlang.com/eunit#testing-regis - then scroll
    %%      to the "Don't Drink Too Much Kool-Aid" sidebar
    timer:sleep(20),
    650 = stop().

ping_test() ->
    ok = start(),
    timer:sleep(10),
    pong = ping(),
    650 = stop().

status_test() ->
    ok = start(),
    timer:sleep(10),
    {ok, 99} = status(99),
    650 = stop().

index_one_test() ->
    ok = start(),
    timer:sleep(10),
    BucketName = "planet-hoth",
    EventName = "basic",
    Payload = "{\"a_field\":\"sign-up; campaign=20130701\"}",
    {ok, _} = index(BucketName, EventName, Payload),
    650 = stop().

index_many_test() ->
    ok = start(),
    timer:sleep(10),
    %% this could be though of as the "product-id", it's a way to group or
    %% partition data within the overall system.
    BucketName = "planet-hoth",
    %% so "basic" will respond to a Lua script named ``basic.lua`` that resides
    %% in the lua_scripts/ in priv that will be used by libmutton, emtn_prog
    %% sets up these paths in its' initialize(char *cwd) founction.  It will
    %% that working directory to the ``code:priv_dir(?APP_NAME)``
    EventName = "basic",
    %% within ``basic.lua``, you'll see that it expects events to be in JSON
    %% format, and that JSON data will has "a_field" key within it.
    P1 = "{\"a_field\":\"sign-up; campaign=20130701\"}",
    P2 = "{\"a_field\":\"TROLOLOL I'm an Event!!!\"}",
    P3 = "{\"a_field\":\"sign-up; campaign=none\"}",
    P4 = "{\"a_field\":\"cancellation; date=20130710\"}",
    %% index will reply back with a tuple that is either:
    %% {ok, Something} for a successful call to index
    %%  or...
    %% {error, Msg, Payaload} for a failure,
    %% More on the failure... the Msg will be the message provided by a call to
    %% mutton_status_get_message() from libmutton.h (the C wrapper for Mutton).
    %% Also, the failed Payload will be provided.  This might not be enough
    %% information to help in triage/troubleshooting.
    {ok, _} = index(BucketName, EventName, P1),
    {ok, _} = index(BucketName, EventName, P2),
    {ok, _} = index(BucketName, EventName, P3),
    {ok, _} = index(BucketName, EventName, P4),

    650 = stop().

index_failure_test() ->
    ok = start(),
    timer:sleep(10),
    BucketName = "planet-hoth",
    EventName = "basic",
    Payload = "{\"snarf_3\":\"sign-up; campaign=20130701\"}",
    {error, _MuttonMsg, _Payload} = index(BucketName, EventName, Payload),

    650 = stop().

-endif.
