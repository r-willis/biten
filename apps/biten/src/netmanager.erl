%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Statistics subsystem.
%%% @end
%%% --------------------------------------------------------------------------

-module(netmanager).

-behaviour(gen_server).

%% API
-export([start_link/0, get_peer_count/0, notify/1, send_all/1, register/1, print/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/config.hrl").

%% Use module name for registered process
-define(SERVER, ?MODULE).

%% @doc Start netmanager process and register it
-spec start_link() -> pid().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%
get_peer_count() ->
    gen_server:call(?SERVER, get_peer_count).

%% @doc Handle addr message
notify({new_peers, L}) ->
    gen_server:cast(?SERVER, {new_peers, L}).

%% @doc Register new peer process
register(Pid) ->
    gen_server:cast(?SERVER, {register, Pid}).

%% @doc Send message to all peers
send_all(Data) ->
    gen_server:cast(?SERVER, {send_all, Data}).

%% @doc Print current peer cout 
print() ->
    gen_server:cast(?SERVER, print).

%get_random_peer() ->
%    gen_server:call(?SERVER, self(), get_random_peer).

%% @doc Stop netmanager process
stop() ->
    gen_server:cast(?SERVER, stop).

%%% ==========================================================================
%%% gen_server callbacks.
%%% ==========================================================================

%% process state
-record(state, {pid_table, peer_count = 0, timer, peer_pool = [], peer_list = []}).

init([]) ->
    T = ets:new('peer_pid', []),
    Timer = timer:send_interval(1000, check_peer_count),
    {ok, #state{pid_table = T, timer = Timer}}.

handle_call(get_peer_count, _From, S) ->
    {reply, {ok, S#state.peer_count}, S};

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

handle_cast({new_peers, L}, S) ->
    N = length(L),
    if N > 0 ->
        %io:format("New peers, N=~b~n", [N]);
        true;
    true     ->
        true
    end,
    %[ supervisor:start_child(peer_sup, [outgoing, {Host, Port}]) || {Host, Port} <- L ],
    %[  {Host, Port}]) || {Host, Port} <- L ],
    L1 = L ++ S#state.peer_pool,
    {noreply, S#state{peer_pool = L1}};

handle_cast({register, Pid}, S) ->
%    io:format("register new peer process: ~p~n", [Pid]),
    L = S#state.peer_list,
    N = S#state.peer_count + 1,
    _Ref = erlang:monitor(process, Pid),
    {noreply, S#state{peer_count = N, peer_list = [Pid|L]}};

handle_cast({send_all, B}, S) ->
    %io:format("sending to all peers ~p bytes~n", [size(B)]),
    [ Pid ! {send, B} || Pid <- S#state.peer_list],
    {noreply, S};

handle_cast(print, S) ->
    io:format("current peers:~p~n", [S#state.peer_count]),
    {noreply, S};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, S) ->
%    io:format("Process ~p down, reason: ~p~n", [Pid, Reason]),
    N = S#state.peer_count - 1,
    L1 = lists:delete(Pid, S#state.peer_list),
    {noreply, S#state{peer_count = N, peer_list = L1}};

handle_info(check_peer_count, S) ->
    N = S#state.peer_count,
    %io:format("Checking peer count, N=~p~n", [N]),
    L = S#state.peer_pool,
    case N < ?MAX_PEERS of
        true  -> 
            {P, L1} = safe_split(?CONN_PER_SECOND, L),
            %io:format("spawning ~p new peers~n", [length(P)]),
            %io:format("~p~n", [P]),
            [ supervisor:start_child(peer_sup, [outgoing, {Host, Port}]) || {Host, Port} <- P ];
        false -> 
            L1 = L
    end,
    {noreply, S#state{peer_pool = L1}}.

terminate(_Reason, _State) ->
    ok.

code_change(_oldVersion, State, _Extra) ->
    {ok, State}.

%%% Local
%% @doc split at most N elements from L
safe_split(N, L) ->
    if length(L) > N -> lists:split(N, L);
               true  -> {L, []}
    end.
