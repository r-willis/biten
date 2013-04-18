%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Statistics subsystem.
%%% @end
%%% --------------------------------------------------------------------------

-module(stat).

-behaviour(gen_server).

%% API
-export([start_link/0, increment/1, increment/2, print/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Use module name for registered process
-define(SERVER, ?MODULE).

%%% ==========================================================================
%%% API
%%% ==========================================================================

%% @doc Start statistics process and register it
-spec start_link() -> pid().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Increments statistics counter by 1
%% -spec increment atom -> ?
increment(Counter) ->
    gen_server:cast(?SERVER, {increment, Counter, 1}).

%% @doc Increments statistics counter by N
%% -spec increment atom -> ?
increment(Counter, N) ->
    gen_server:cast(?SERVER, {increment, Counter, N}).

%% @doc Print current statistics to stdout
print() ->
    gen_server:cast(?SERVER, print).

%% @doc Stop statistics process
stop() ->
    gen_server:cast(?SERVER, stop).

%%% ==========================================================================
%%% gen_server callbacks.
%%% ==========================================================================

%% process state
-record(state, {connects = 0, 
                connected = 0, 
                accepted = 0, 
                relayed_tx = 0,
                error_timedout = 0, 
                error_connrefused = 0,
                error_hostunreach = 0,
                error = 0,
                answer = 0,
                start_time
                }).

init([]) ->
    {ok, #state{start_time = calendar:universal_time()}}.

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

-define(INC(A, B, C), A{B = A.B + C}).

handle_cast({increment, Counter, N}, S) ->
    case Counter of 
        %connect   -> {noreply, S#state{connects = S#state.connects + N}};
        connect    -> {noreply, ?INC(S#state, connects, N)};
        connected  -> {noreply, ?INC(S#state, connected, N)};
        accepted   -> {noreply, ?INC(S#state, accepted, + N)};
        relayed_tx -> {noreply, ?INC(S#state, relayed_tx,  N)};
        {error, etimedout}    -> {noreply, ?INC(S#state, error_timedout, N)};
        {error, econnrefused} -> {noreply, ?INC(S#state, error_connrefused, N)};
        {error, ehostunreach} -> {noreply, ?INC(S#state, error_hostunreach, N)};
        {error, _}            -> {noreply, ?INC(S#state, error, N)};

        answer    -> {noreply, S#state{answer = S#state.answer + N}}
    end;

handle_cast(print, S) ->
    io:format("Start time..~s UTC~n", [util:strftime(S#state.start_time)]),
    io:format("Cur time....~s UTC~n", [util:strftime(calendar:universal_time())]),
    io:format("Connects....~b~n", [S#state.connects]),
    io:format("Connected...~b~n", [S#state.connected]),
    io:format("Accepted....~b~n", [S#state.accepted]),
    io:format("Answers.....~b~n", [S#state.answer]),
    {ok, Npeer} = netmanager:get_peer_count(),
    io:format("Cur peers...~b~n", [Npeer]),
    io:format("Relayed TX..~b~n", [S#state.relayed_tx]),
    io:format("Mempool:~n", []),
    try mempool:get_stats() of
        {N1, N2, N3, N4, T1, T2, T3, T4} ->
            io:format(" inv table..~b~n", [N1]),
            io:format(" req table..~b~n", [N2]),
            io:format(" tx table...~b~n", [N3]),
            io:format(" peer table.~b~n", [N4]),
            io:format(" check inv..~.4f s~n", [T1]),
            io:format(" clean inv..~.4f s~n", [T2]),
            io:format(" clean req..~.4f s~n", [T3]),
            io:format(" clean tx...~.4f s~n", [T4])
    catch
        Error:Reason ->
            io:format(" exception: ~p:~p~n", [Error, Reason])
    end,
    io:format("Connection_errors:~n", []),
    io:format(" timeout....~p~n", [S#state.error_timedout]), 
    io:format(" refused....~p~n", [S#state.error_connrefused]), 
    io:format(" unreach....~p~n", [S#state.error_hostunreach]), 
    io:format(" other......~b~n", [S#state.error]),
    io:format("Chain:~n", []),
    try chain:get_stats() of
        {N, Size, NReq} ->
            io:format(" obj count..~b~n", [N]),
            io:format(" size.......~.1f MB~n", [Size/1024/1024]),
            io:format(" requests...~b~n", [NReq])
    catch
        Error1:Reason1 ->
            io:format(" exception: ~p:~p~n", [Error1, Reason1])
    end,
    {noreply, S};
 

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_oldVersion, State, _Extra) ->
    {ok, State}.
