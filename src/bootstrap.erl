%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Bootstrap process. Uses DNS for peer discovery.
%%% @end
%%% --------------------------------------------------------------------------

-module(bootstrap).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Use module name for registered process
-define(SERVER, ?MODULE).

%%% ==========================================================================
%%% API
%%% ==========================================================================

%% @doc Start bootstrap process and register it
-spec start_link() -> pid().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop statistics process
stop() ->
    gen_server:cast(?SERVER, stop).

%%% ==========================================================================
%%% gen_server callbacks.
%%% ==========================================================================

%% process state
-record(state, {none}).

init([]) ->
    gen_server:cast(?SERVER, go),
    {ok, #state{}}.

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

handle_cast(go, S) ->
    IPs = get_addrs_ipv4_dns(),
    peerdiscovery:add([{IP, 8333} || IP <- IPs]),
    {stop, normal, S};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_oldVersion, State, _Extra) ->
    {ok, State}.

%%% ===========================================
%%% Local functions
%%% ===========================================

%% @doc Get addrs for bootstrap from DNS.
get_addrs_ipv4_dns() ->
    L = ["bitseed.xf2.org",
         "dnsseed.bluematt.me",
         "seed.bitcoin.sipa.be",
         "dnsseed.bitcoin.dashjr.org"
        ],
    lists:flatten([nslookup_ipv4(A) || A <- L]).

nslookup_ipv4(Addr) ->
    Type = a,
    Class = in,
    case inet_res:resolve(Addr, Class, Type) of
        {ok,Msg} ->
            [inet_dns:rr(RR, data) ||
                RR <- inet_dns:msg(Msg, anlist),
                inet_dns:rr(RR, type) =:= Type,
                inet_dns:rr(RR, class) =:= Class];
        {error,_} ->
            []
    end.
