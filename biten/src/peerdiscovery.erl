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
-record(state, {peers, t_peer, t_rand}).

init([]) ->
    T_peer = ets:new(peer, [set]),
    T_rand = ets:new(rand, [set]),
    {ok, #state{t_peer = T_peer, t_rand = T_rand}}. 

handle_call(get, _From, S) ->
    L = [ X || [X] <- ets:match(S#state.t_peer, '$1')],
    {reply, L, S}.

handle_cast({add, L}, S) ->
    L_new = [ X || {_, P} = X <- L, P > 0, ets:lookup(S#state.t_peer, X) =:= [] ],
    [ ets:insert(S#state.t_peer, {X}) || X <- L_new ],
    netmanager:notify({new_peers, L_new}),
    {noreply, S};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_oldVersion, State, _Extra) ->
    {ok, State}.
