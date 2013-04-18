%%% ---------------------------------------------------------------------------
%%% Blockchain service
%%% ---------------------------------------------------------------------------

-module(chain).

-behaviour(gen_server).

%% API fucntions
-export([start_link/0, get_stats/0, get_chain/1, got_inv/2, got_headers/1, got_getdata/1, stop/0]).

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

%% @doc get list of headers from root to given hash 
get_chain(H) ->
    gen_server:call(?SERVER, {get_chain, H}, 100*1000).

%% @doc called when getdata received
got_getdata(L) ->
    gen_server:cast(?SERVER, {got_getdata, self(), L}).

%% @doc called when block headers received
got_headers(L) ->
    gen_server:cast(?SERVER, {got_headers, self(), L}).

%% @doc called when blcok invs received
got_inv(P, L) ->
    gen_server:cast(?SERVER, {got_inv, P, L}).

%% @doc Stop blockchain process
stop() ->
    gen_server:cast(?SERVER, stop).

%%% ==========================================================================
%%% gen_server callbacks.
%%% ==========================================================================

%% process state
-record(state, {t_chain, req = []}).

init([]) ->
    {ok, _} = dets:open_file(raw, [{file, "data/raw.dets"}]),
    self() ! check_headers,
    {ok, #state{}}. 

handle_call(get_stats, _From, S) ->
    N = dets:info(raw, no_keys),
    Size = dets:info(raw, file_size),
    NReq = length(S#state.req),
    {reply, {N, Size, NReq}, S};

handle_call({get_chain, H}, _From, S) ->
    {reply, chain(dir, H), S};

handle_call(_, _From, S) ->
    {reply, ok, S}.

handle_cast({got_headers, P, L}, S) ->
    lists:foreach(fun({P1, _, T}) ->
                    case P1 of
                        P ->
                            %io:format("cancel timer, peer = ~p~n", [P]),
                            timer:cancel(T);
                        _ ->
                            ok
                    end
                  end, S#state.req),
    NewReqs = [ X || {P1, _R, _T} = X <- S#state.req, P1 =/= P ],
    %NewBlocks = [ {b_crypto:hash(H), H} || H <- L, not dets:member(raw, {header, b_crypto:hash(H)}) ],
    {_OldBlocks, NewBlocks} = lists:partition(fun({HH, _}) -> dets:member(raw, {header, HH}) end,
        [ {b_crypto:hash(H), H} || H <- L]),
    %io:format("headers from ~p: ~b new, ~b old~n", [P, length(NewBlocks), length(OldBlocks)]),
    %[ io:format("  ~p ~s~n", [dets:member(raw, {header, b_crypto:hash(H)}), protocol:binary_to_hexstr(b_crypto:hash(H), "")]) || H <- L ],
    [ dets:insert(raw, {{header, HH}, H}) || {HH, H} <- NewBlocks ],
    [ begin {_, Prev, _, _, _, _} = protocol:parse_block_header(H), update_dir(dir, HH, Prev ) end || {HH, H} <- NewBlocks ],
    {noreply, S#state{req = NewReqs}};

handle_cast({got_inv, P, L}, S) ->
    New = [ H || H <- L, dets:lookup(raw, {header, H}) =:= [] ],
    %Old = [ H || H <- L, dets:lookup(raw, {header, H}) =/= [] ],
    %io:format("new blocks inv from ~p~n", [P]),
    %[ io:format("  ~s~n", [protocol:binary_to_hexstr(H, "")]) || H <- New ],
    [ netmanager:send(P, protocol:getheaders_msg([], H)) || H <- New ],
    %io:format("old blocks inv from ~p~n", [P]),
    %[ io:format("  ~s~n", [protocol:binary_to_hexstr(H, "")]) || H <- Old ],
    {noreply, S};

handle_cast({got_getdata, _P, _L}, S) ->
    %io:format("chain:got_getdata from ~p: ~p~n", [P, length(L)]),
    {noreply, S};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(check_headers, S) ->
    T1 = S#state.t_chain,
    {T2, CurReq} = case T1 of
        undefined ->
            T = true,
            dets:open_file(dir, [{file, "data/dir.dets"}, {ram_file, false}]),
            %dets:open_file(dir, [{file, "data/dir.dets"}, {ram_file, true}]),
            %io:format("cleaning index~n"),
            dets:delete_all_objects(dir),
            dets:insert(dir, {roots, []}),
            dets:insert(dir, {leaves, []}),
            %io:format("building index~n"),
            dets:traverse(raw, fun(X) ->
                                    case X of
                                        {{header, H}, B} ->
                                            P = protocol:prev_block_header(B),
                                            update_dir_only(dir, H, P),
                                            ok;
                                         _ ->
                                            ok
                                        end,
                                    continue
                                    end),

            %io:format("done~n"),
            %io:format("calculating roots and leaves~n"),
            dets:traverse(dir, fun({{prev, A}, B}) ->
                                      case dets:member(dir, {prev, B}) of
                                          false ->
                                              dets:insert(dir, {roots, [A | roots(dir)]});
                                          true ->
                                              ok
                                      end,
                                      case dets:member(dir, {next, A}) of
                                          false ->
                                              dets:insert(dir, {leaves, [A | leaves(dir)]});
                                          true ->
                                              ok
                                      end,
                                      continue;
                                  (_) ->
                                      continue
                                end),
            %io:format("done~n"),
            {T, S#state.req};
        _ ->
            %io:format("Current requsets: ~b~n", [length(S#state.req)]),
            L = [{prev, X} || X <- roots(dir)] ++ [{cont, X} || X <- leaves(dir)],
            {ok, Peers} = netmanager:get_peer_list(),
            FreePeers = Peers -- [P || {P, _, _} <- S#state.req], % peers that do not have pending requests
            PendingReqs = [ R || {_, R, _} <- S#state.req], % pending requests
            PrioReqs1 = [ {this, X} || X <- [?GENESIS_BLOCK_HASH|?CHECKPOINTS],
                                        not dets:member(raw, {header, X})] -- PendingReqs,
            Prio = util:random_match(FreePeers, PrioReqs1),
            {PrioPeers, PrioReqs} = lists:unzip(Prio),
            L1 = L -- [{prev, ?GENESIS_BLOCK_HASH}], % there are no blocks before genesis block 
            Normal = util:random_match(FreePeers -- PrioPeers, L1 -- PendingReqs -- PrioReqs),
            %io:format("request list:~n"),
            %[ io:format("   ~p ~p ~s~n", [Peer, Type, util:bin_to_hex(H)]) || {Peer, {Type, H}, _} <- S#state.req ],
            L2 = [ok || {_, {cont, _}, _} <- S#state.req],
            K = max(4 - length(L2),  0),
            {ContReqs, OtherReqs} = lists:partition(fun({_, {cont, _}}) -> true;
                                                      (_) -> false end, Normal),
            Reqs = Prio ++ util:take_random(K, ContReqs) ++ OtherReqs,
            %io:format("request list new:~n"),
            %[ io:format("   ~p ~p ~s~n", [Peer, Type, util:bin_to_hex(H)]) || {Peer, {Type, H}} <- Reqs ],
            lists:foreach(fun({Peer, {this, Hash}}) ->
                                    netmanager:send(Peer, protocol:getheaders_msg([], Hash));
                             ({Peer, {prev, Hash}}) ->
                                    PrevHash = prev_block(dir, Hash),
                                    netmanager:send(Peer, protocol:getheaders_msg([], PrevHash));
                             ({Peer, {cont, Hash}}) ->
                                    netmanager:send(Peer, protocol:getheaders_msg([Hash], <<0:32/unit:8>>))
                          end, Reqs),
            {T1, S#state.req ++ [ begin {ok, T} = timer:send_after(10000 + random:uniform(10000), {timeout, P, R}), {P, R, T} end || {P, R} <- Reqs ]}
    end,
    %io:format("checking headers~n", []),
    timer:send_after(2000, check_headers),
    {noreply, S#state{t_chain = T2, req = CurReq}};

handle_info({timeout, P, R}, S) ->
    NewReq = [X || {P1, R1, _} = X <- S#state.req, {P1, R1} =/= {P, R}],
    {noreply, S#state{req = NewReq}};

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_oldVersion, State, _Extra) ->
    {ok, State}.

%%% Local

update_dir(T, H, P) ->
    dets:insert(T, {{prev, H}, P}),
    dets:insert(T, {{next, P}, lists:usort([H | next_block(T, P)])}),
    Leaves = leaves(T),
    Roots  = roots(T),
    Leaves1 = Leaves -- [P],
    Roots1  = Roots  -- next_block(T, H),
    Leaves2 = case next_block(T, H) of
        [] ->
            lists:usort([H | Leaves1]);
        [_] ->
            Leaves1
    end,
    Roots2 = case prev_block(T, P) of 
        undefined ->
            lists:usort([H | Roots1]);
        _ ->
            Roots1
    end,
    dets:insert(T, {roots, Roots2}),
    dets:insert(T, {leaves, Leaves2}),
    ok.

update_dir_only(T, H, P) ->
    dets:insert(T, {{prev, H}, P}),
    dets:insert(T, {{next, P}, [H | next_block(T, P)]}),
    ok.


prev_block(T, H) ->
    case dets:lookup(T, {prev, H}) of
        [] ->
            undefined;
        [{_, P}] ->
            P
    end.

next_block(T, H) ->
    case dets:lookup(T, {next, H}) of
        [] ->
            [];
        [{_, L}] ->
            L
    end.

lookup(T, What) ->
    case dets:lookup(T, What) of
        [] ->
            [];
        [{_, R}] ->
            R
    end.

roots(T) ->
    lookup(T, roots).

leaves(T) ->
    lookup(T, leaves).

chain(T, H) ->
    chain(T, H, []).

chain(T, H, Acc) ->
    case prev_block(T, H) of
        undefined ->
            Acc;
        Prev ->
            chain(T, Prev, [H | Acc])
    end.
