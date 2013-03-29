%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Accepter process - accepts incoming connections from peers.
%%% @end
%%% --------------------------------------------------------------------------

-module(accepter).

-behaviour(gen_server).

%% API
-export([start_link/0, transfer_ownership/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Use module name for registered process
-define(SERVER, ?MODULE).

%%% ==========================================================================
%%% API
%%% ==========================================================================

%% @doc Start acceptor process and register it
-spec start_link() -> pid().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Transfer socket membership to calling process
transfer_ownership(Sock) ->
    %io:format("transfer1~n", []),
    gen_server:call(?SERVER, {transfer_ownership, Sock}).

%% @doc Stop acceptor process
stop() ->
    gen_server:cast(?SERVER, stop).

%%% ==========================================================================
%%% gen_server callbacks.
%%% ==========================================================================

%% process state
-record(state, {none}).

init([]) ->
    {ok, LSock} = gen_tcp:listen(8333, [binary, {packet, 0}, 
                                            {active, false}, {reuseaddr, true}]),
    {ok, _Ref} = async_accept(LSock),
    {ok, #state{}}.

handle_call({transfer_ownership, Sock}, {Pid, _}, S) ->
    gen_tcp:controlling_process(Sock, Pid),
    {reply, ok, S};

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({inet_async, LSock, _, {ok, Sock}}, S) ->
    supervisor:start_child(peer_sup, [incoming, Sock]),
    {ok, _Ref} = async_accept(LSock),
    {noreply, S};

handle_info(Msg, S) ->
    io:format("~p~n", [Msg]),
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_oldVersion, State, _Extra) ->
    {ok, State}.

%% Internal

async_accept(Sock) ->
    Pid = self(),
    proc_lib:spawn_link(fun() -> accept_once(Sock, Pid) end),
    {ok, undefined}.

accept_once(LSock, ParentPid) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    gen_tcp:controlling_process(Sock, ParentPid),
    %io:format("R = ~p~n", [R]),
    ParentPid ! {inet_async, LSock, undefined, {ok, Sock}},
    {normal, ok}.
