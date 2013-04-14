%%% ---------------------------------------------------------------------------
%%% Blockchain service
%%% ---------------------------------------------------------------------------

-module(chain).

-behaviour(gen_server).

%% API fucntions
-export([start_link/0, get_stats/0, got_headers/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/config.hrl").

%% Use module name for registered process
-define(SERVER, ?MODULE).

%%% ===========================================================================
%%% API
%%% ===========================================================================

%% @doc start blockchain service and register it
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc get stats
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc called when getdata received
got_getdata(L) ->
    gen_server:cast(?SERVER, {got_getdata, self(), L}).

%% @doc called when block headers received
got_headers(L) ->
    gen_server:cast(?SERVER, {got_headers, self(), L}).

%% @doc Stop blockchain process
stop() ->
    gen_server:cast(?SERVER, stop).

%%% ==========================================================================
%%% gen_server callbacks.
%%% ==========================================================================

%% process state
-record(state, {}).

init([]) ->
    {ok, _} = dets:open_file(raw, [{file, "data/raw.dets"}]),
    self() ! check_headers,
    {ok, #state{}}. 

handle_call(get_stats, _From, S) ->
    N = dets:info(raw, no_keys),
    Size = dets:info(raw, file_size),
    {reply, {N, Size}, S};

handle_call(_, _From, S) ->
    {reply, ok, S}.

handle_cast({got_headers, P, L}, S) ->
    [ dets:insert(raw, {{header, b_crypto:hash(H)}, H}) || H <- L ],
    io:format("headers from ~p~n", [P]),
    [ io:format("  ~s~n", [protocol:binary_to_hexstr(b_crypto:hash(H), "")]) || H <- L ],
    {noreply, S};

handle_cast({got_getdata, P, L}, S) ->
    io:format("chain:got_getdata from ~p: ~p~n", [P, length(L)]),
    {noreply, S};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(check_headers, S) ->
    %io:format("checking headers~n", []),
    timer:send_after(15000, check_headers),
    {noreply, S};

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_oldVersion, State, _Extra) ->
    {ok, State}.

%%% Local
