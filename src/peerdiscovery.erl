%%% ---------------------------------------------------------------------------
%%% Peer discovery service
%%% ---------------------------------------------------------------------------

-module(peerdiscovery).

-behaviour(gen_server).

%% API fucntions
-export([start_link/0, add/1, get/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Use module name for registered process
-define(SERVER, ?MODULE).

%%% ===========================================================================
%%% API
%%% ===========================================================================

%% @doc start peer discovery service and register it
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc add announced peers to pool
-spec add(list()) -> atom().
add(L) ->
    gen_server:cast(?SERVER, {add, L}).

%% @doc get all known peer addresses
get() ->
    gen_server:call(?SERVER, get).

%% @doc Stop peer discovery process
stop() ->
    gen_server:cast(?SERVER, stop).

%%% ==========================================================================
%%% gen_server callbacks.
%%% ==========================================================================

%% process state
-record(state, {peers}).

init([]) ->
    {ok, #state{peers = sets:new()}}. 

handle_call(get, _From, S) ->
    {reply, sets:to_list(S#state.peers), S}.

handle_cast({add, L}, S) ->
    L1 = lists:filter(fun({_, P}) -> P > 0 end, L),
    NewSet = sets:from_list(L1),
    netmanager:notify({new_peers, 
        sets:to_list(sets:subtract(
            NewSet, S#state.peers))}),
    NewPeers = sets:union(S#state.peers, NewSet),
    {noreply, S#state{peers = NewPeers}};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_oldVersion, State, _Extra) ->
    {ok, State}.
