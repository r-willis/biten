%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Peer handling - all peer communication happens here.
%%% @end
%%% --------------------------------------------------------------------------

-module(peer).

%% API
-export([start_link/2, connect/1, connect/2, incoming/1]).

%% Process state
-record(peer, {state=new, host, port, socket, direction = outgoing, rest = <<>>, t_send}).

connect(Addrs) -> 
    [ spawn_link(?MODULE, connect, [H, P]) || {H, P} <- Addrs ].

start_link(outgoing, {Host, Port}) ->
    {ok, proc_lib:spawn_link(?MODULE, connect, [Host, Port])};

start_link(incoming, Sock) ->
    {ok, proc_lib:spawn_link(?MODULE, incoming, [Sock])}.

incoming(Sock) ->
    accepter:transfer_ownership(Sock),
    loop(#peer{state = new, direction = incoming, socket = Sock}).

connect(Host, Port) ->
    %io:format("Connecting to ~p:~p~n", [Host, Port]),
    stat:increment(connect),
    try gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
            loop(#peer{host=Host, port=Port, socket=Socket});
        {error, R} ->
            stat:increment({error, R})
    catch
        Error:Reason -> stat:increment({error, {Error, Reason}})
    end.

loop(#peer{state = new, direction = incoming} = P) ->
    stat:increment(accepted),
    inet:setopts(P#peer.socket, [{active, once}]),
    loop(P#peer{state = loop});
    
loop(#peer{state = new} = P) ->
    stat:increment(connected),
   % io:format("Connected to ~s:~p, sending version~n", [util:ip_to_str(P#peer.host), P#peer.port]),
    gen_tcp:send(P#peer.socket, protocol:version_msg()),
    loop(P#peer{state = version_sent});

loop(#peer{state = version_sent} = P) ->
    case gen_tcp:recv(P#peer.socket, 0, 15*1000) of
        {ok, B} -> 
            R = B;
        {error, _} -> 
            R = <<>>,
            loop(P#peer{state = stop})
    end,
    stat:increment(answer),
    %io:format("Version response from ~s:~p:~n", [util:ip_to_str(P#peer.host), P#peer.port]),
    gen_tcp:send(P#peer.socket, protocol:getaddr_msg()),
    %gen_tcp:send(P#peer.socket, protocol:ping_msg()),
    Rest = P#peer.rest,
    inet:setopts(P#peer.socket, [{active, once}]),
    netmanager:register(self()),
    loop(P#peer{state = loop, rest = <<Rest/bytes, R/bytes>>, t_send = now()});

loop(#peer{state = stop}) ->
    %io:format("stopping peer process~n", []),
    {normal, ok};

loop(#peer{state = loop} = P) ->
    Socket = P#peer.socket,
    Host = P#peer.host,
    Rest = P#peer.rest,
    receive
        {tcp, _, R} ->
            D = << Rest/binary, R/binary>>,
            case protocol:get_message(D) of
                {ok, Hdr, Payload, NewRest} ->
                    {ok, _, Cmd, _, CRC} = protocol:parse_header(Hdr),
                    case lists:member(Cmd, [unknown]) of
                        true ->
                            F = io_lib:format("dump/dump_~s_~s.bin", [util:ip_to_str(Host), util:crc_to_str(CRC)]),
                            file:write_file(F, [Hdr, Payload]);
                        false -> ok
                    end,
                    case Cmd of 
                        version when P#peer.direction =:= incoming ->
                            gen_tcp:send(P#peer.socket, protocol:version_msg()),
                            gen_tcp:send(P#peer.socket, protocol:verack_msg()),
                            gen_tcp:send(P#peer.socket, protocol:addr_msg()),
                            netmanager:register(self());
                            %io:format("Got version from incoming connection~n", []);
                        version when P#peer.direction =:= outgoing ->
                            gen_tcp:send(P#peer.socket, protocol:verack_msg()),
                            gen_tcp:send(P#peer.socket, protocol:addr_msg());
                        inv ->
                            {ok, _N, L} = protocol:parse_inv(Payload),
                            mempool:got_inv(L);
                        addr ->
                            {ok, _N, L} = protocol:parse_addr(Payload),
                            %io:format("Got addr from ~s, n=~b (read ~b).~n", [util:ip_to_str(Host), N, length(L)]),
                            peerdiscovery:add(L);
                        ping ->
                            io:format("ping received from ~s:~p~n", [util:ip_to_str(Host), P#peer.port]),
                            protocol:pong_msg(Payload);
                          tx ->
                            mempool:got_tx(Payload);
                     getdata -> 
                            {ok, _N, L} = protocol:parse_getdata(Payload),
                            mempool:got_getdata(L);
                           _ ->
                             ok
                    end;
                {error, incomplete} ->
                    case size(D) > 2*1024*1024 of
                        true -> loop(P#peer{state = stop});
                        false -> ok
                    end,
                    NewRest = D
            end,
            inet:setopts(P#peer.socket, [{active, once}]),
            loop(P#peer{rest = NewRest});
        {tcp_closed, _} ->
            %io:format("connection closed, peer terminated~n", []),
            loop(P#peer{state = stop});
        {send, B} ->
            %io:format("sending ~p bytes~n", [size(B)]),
            gen_tcp:send(Socket, B),
            loop(P)
    end.
