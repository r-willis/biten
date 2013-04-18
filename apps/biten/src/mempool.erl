%%% ---------------------------------------------------------------------------
%%% Memory pool for transactions
%%% ---------------------------------------------------------------------------

-module(mempool).

-behaviour(gen_server).

%% API fucntions
-export([start_link/0, pause/1, get_stats/0, got_inv/1, got_tx/1, got_getdata/1, print/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/config.hrl").

%% Use module name for registered process
-define(SERVER, ?MODULE).

%%% ===========================================================================
%%% API
%%% ===========================================================================

%% @doc start peer discovery service and register it
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc add inventory element to pool 
-spec got_inv(list()) -> atom().
got_inv(L) ->
    gen_server:cast(?SERVER, {got_inv, self(), L, now()}).

%% @doc called by peer module when tx received
got_tx(B) ->
    gen_server:cast(?SERVER, {got_tx, B, now()}).

%% @doc called by peer module when getdata received
got_getdata(L) ->
    gen_server:cast(?SERVER, {got_getdata, self(), L}).

%% @doc print contents of memory pool 
print() ->
    gen_server:cast(?SERVER, print).

%% @doc Pause memory pool process for N milliseconds; useful for testing
pause(N) ->
    gen_server:cast(?SERVER, {pause, N} ).

%% @doc Stop memory pool process
stop() ->
    gen_server:cast(?SERVER, stop).

%%% ==========================================================================
%%% gen_server callbacks.
%%% ==========================================================================

%% process state
-record(state, {t_inv, t_req, t_tx, t_peer, clean_tx_time, clean_inv_time, clean_req_time, check_inv_time, log}).

init([]) ->
    T = ets:new(inv, [bag]),
    T2 = ets:new(request, [set]),
    T3 = ets:new(tx, [set]),
    T4 = ets:new(peer, [bag]),
    self() ! check_inv,
    self() ! clean_inv,
    self() ! clean_req,
    self() ! clean_tx,
    {ok, Log} = file:open("log.txt", [write]),
    {ok, #state{t_inv = T, t_req = T2, t_tx = T3, t_peer = T4, log = Log}}. 

handle_call(get_stats, _From, S) ->
    N1 = ets:info(S#state.t_inv, size),
    N2 = ets:info(S#state.t_req, size),
    N3 = ets:info(S#state.t_tx, size),
    N4 = ets:info(S#state.t_peer, size),
    T1 = S#state.check_inv_time,
    T2 = S#state.clean_inv_time,
    T3 = S#state.clean_req_time,
    T4 = S#state.clean_tx_time,
    %timer:sleep(10000), % test case for timeou handling
    {reply, {N1, N2, N3, N4, T1, T2, T3, T4}, S};

handle_call(_, _From, S) ->
    {reply, ok, S}.

handle_cast({got_inv, P, L, Time}, S) ->
    T_INV = S#state.t_inv,
    T_TX = S#state.t_tx,
    T_PEER = S#state.t_peer,
    N = length(ets:lookup(T_PEER, P)),
    L1 = [ X || {Type, Hash} = X <- L, Type =:= 1, ets:lookup(T_TX, Hash) =:= []],
    L1_BLOCKS = [ Hash || {Type, Hash} <- L, Type =:= 2],
    case length(L1_BLOCKS) of
        0 ->
            ok;
        _ ->
            chain:got_inv(P, L1_BLOCKS)
    end,
    %% Relying on the fact that N =< MAX_INV_PER_PEER (at list, should be)
    L2 = lists:sublist(L1, ?MAX_INV_PER_PEER - N),
    [ ets:insert(T_INV, {Hash, Type, P, Time}) || {Type, Hash} <- L2 ],
    [ ets:insert(T_PEER, {P, Hash}) || {_, Hash} <- L2 ],
    {MegaSec, Sec, MicroSec} = now(),
    io:format(S#state.log, "~b:~b:~b inv ~p ~b ~b~n", [MegaSec, Sec, MicroSec, P, length(L), N]),
    %[ io:format(S#state.log, "~b:~b:~b inv1 ~b ~p ~s~n", [MegaSec, Sec, MicroSec, P, T, protocol:binary_to_hexstr(H, "")]) || {T, H} <- L],
    {noreply, S};

handle_cast({got_getdata, P, L}, S) ->
    %io:format("mempool:got_getdata from ~p: ~p~n", [P, length(L)]),
    T_TX = S#state.t_tx,
    TX_L = [ ets:lookup(T_TX, Hash) || {1, Hash} <- L ],
    N = length(TX_L),
    stat:increment(relayed_tx, N),
    %io:format("found tx: ~p~n", [N]),
    [ P ! {send, protocol:tx_msg(TX)} || [{_, TX, _}] <- TX_L ],
    {noreply, S};

handle_cast({got_tx, B, Time}, S) ->
    T_INV = S#state.t_inv,
    T_REQ = S#state.t_req,
    T_TX = S#state.t_tx,
    T_PEER = S#state.t_peer,
    Hash = b_crypto:hash(B),
    %io:format("GOT TX, requests: ~p~n", [ets:lookup(T_REQ, Hash)]),
    case (ets:lookup(T_REQ, Hash) =/= []) of
        true ->
            %io:format("Got known tx, removing requests~n", []),
            [ ets:delete_object(T_PEER, {Pid, H})  || {H, _, Pid, _} <- ets:lookup(T_INV, Hash) ],
            ets:delete(T_INV, Hash),
            ets:delete(T_REQ, Hash),
            ets:insert(T_TX, {Hash, B, Time}),
            netmanager:send_all(protocol:inv_msg([{1, Hash}]));
        false ->
            %io:format("Got unknown tx, ignoring~n", [])
            ok
    end,
    {noreply, S};

handle_cast(print, S) ->
    T_INV = S#state.t_inv,
    T_REQ = S#state.t_req,
    T_TX = S#state.t_tx,
    io:format("INV TABLE ==================~n", []),
    [ io:format("~s | ~b | ~p | ~p~n", [protocol:binary_to_hexstr(Hash, ""), Type, Pid, Time]) || [{Hash, Type, Pid, Time}] <- ets:match(T_INV, '$1')],
    io:format("REQ TABLE ==================~n", []),
    [ io:format("~s | ~b | ~p | ~p~n", [protocol:binary_to_hexstr(Hash, ""), Type, Pid, Time]) || [{Hash, Type, Pid, Time}] <- ets:match(T_REQ, '$1')],
    io:format("TX TABLE -==================~n", []),
    [ io:format("~s | ~p~n", [protocol:binary_to_hexstr(Hash, ""), Time]) || [{Hash, _, Time}] <- ets:match(T_TX, '$1')],
    io:format("TX table entries: ~p~n", [length(ets:match(T_TX, '$1'))]),
    {noreply, S};

handle_cast({pause, T}, S) ->
    timer:sleep(T),
    {noreply, S};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(check_inv, S) ->
    %io:format("checking inventory table~n", []),
    T_INV = S#state.t_inv,
    T_REQ = S#state.t_req,
    Now = now(),
    R = requests(ets:match(T_INV, '$1'), T_REQ, []),
    [ Pid ! {send, protocol:getdata_msg([{Hash, Type}])} || {Hash, Type, Pid} <- R],
    %io:format("requests: ~p~n", [C]),
    timer:send_after(5000, check_inv),
    %io:format("time check_inv = ~.4f~n", [timer:now_diff(now(), Now)*1.0e-6]),
    {noreply, S#state{check_inv_time = timer:now_diff(now(), Now)*1.0e-6}};

handle_info(clean_inv, S) ->
    T_INV = S#state.t_inv,
    T_PEER = S#state.t_peer,
    Now = now(),
    clean_inv(lists:sort(fun({H1, _, _, T1}, {H2, _, _, T2}) -> (H1 > H2) or (H1 =:= H2) and (T1 > T2) end,
        [ X || [X] <- ets:match(T_INV, '$1')]), T_INV, T_PEER, 0, undefined),
    timer:send_after(6000, clean_inv),
    %io:format("time clean_inv = ~.4f~n", [timer:now_diff(now(), Now)*1.0e-6]),
    {noreply, S#state{clean_inv_time = timer:now_diff(now(), Now)*1.0e-6}};

handle_info(clean_req, S) ->
    T_INV = S#state.t_inv,
    T_REQ = S#state.t_req,
    T_PEER = S#state.t_peer,
    Now = now(),
    TimedOut = [ X || [{_, _, _, Time} = X] <- ets:match(T_REQ, '$1'), timer:now_diff(Now, Time) > 20*1000000 ],
    [ ets:delete(T_REQ, Hash) || {Hash,_ ,_ ,_ } <- TimedOut ],
    %% delete from inventory table - next time we will try other peer
    [ ets:match_delete(T_INV, {Hash, Type, Pid, '_'}) || {Hash, Type, Pid, _ } <- TimedOut ],
    [ ets:delete_object(T_PEER, {Pid, Hash}) || {Hash, _Type, Pid, _ } <- TimedOut ],
    %% re-arm timer
    timer:send_after(10000, clean_req),
    %io:format("time clean_req = ~.4f~n", [timer:now_diff(now(), Now)*1.0e-6]),
    {noreply, S#state{clean_req_time = timer:now_diff(now(), Now)*1.0e-6}};

handle_info(clean_tx, S) ->
    T_TX = S#state.t_tx,
    Now = now(),
    [ ets:delete(T_TX, Hash) || [{Hash, _, Time }] <- 
           ets:match(T_TX, '$1'), timer:now_diff(Now, Time) > 600*1000000 ],
    timer:send_after(60000, clean_tx),
    %io:format("time clean_tx = ~.4f~n", [timer:now_diff(now(), Now)*1.0e-6]),
    {noreply, S#state{clean_tx_time = timer:now_diff(now(), Now)*1.0e-6}};


handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_oldVersion, State, _Extra) ->
    {ok, State}.

%%% Local

%% Warining - side-effects on ets
requests([], _Tab, Res) ->
     Res;

requests([[{Hash, Type, Pid, _Time}]|T], Tab, Res) ->
    case ets:lookup(Tab, Hash) of
        [] ->
            ets:insert(Tab, {Hash, Type, Pid, now()}),
            requests(T, Tab, [{Hash, Type, Pid}|Res]); 
         _ ->
            requests(T, Tab, Res)
    end.

clean_inv([], _Tab, _TabPeer, _N, _Prev) ->
    ok;

clean_inv([{Hash, _, Pid, Time} = Obj|T], Tab, TabPeer, N, Prev) ->
    N1 = if Hash =:= Prev -> N+1; true -> 1 end,
    Now = now(),
    case (N1 > 10) or (timer:now_diff(Now, Time) > 600*1000000) of
        true ->
            ets:delete_object(Tab, Obj),
            ets:delete_object(TabPeer, {Pid, Hash});
        false ->
            ok
    end,
    clean_inv(T, Tab, TabPeer, N1, Hash).
