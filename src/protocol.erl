%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Protocol related bits - message construction and parsing 
%%% @end
%%% --------------------------------------------------------------------------

-module(protocol).
-compile([export_all]).

-define(MAIN_MAGIC, <<16#F9BEB4D9:32/big>>).
-define(MAGIC, ?MAIN_MAGIC).

hexstr_to_binary(S) ->
    T = string:tokens(S, " "),
    list_to_binary([list_to_integer(X, 16) || X <- T]).

binary_to_hexstr(B, D) ->
    string:join([io_lib:format("~2.16.0b", [X]) || <<X>> <= B ], D).

binary_to_hexstr(B) ->
    binary_to_hexstr(B, " ").

make_message(Magic, Command, Payload) ->
    Len = byte_size(Payload),
    CRC = b_crypto:crc(Payload),
    CommandBin = case Command of
        version -> atom_to_cmd(version);
        getaddr -> atom_to_cmd(getaddr);
        getdata -> atom_to_cmd(getdata);
         verack -> atom_to_cmd(verack);
           addr -> atom_to_cmd(addr);
           ping -> atom_to_cmd(ping);
           pong -> atom_to_cmd(pong);
            inv -> atom_to_cmd(inv);
             tx -> atom_to_cmd(tx)
    end,
    <<Magic/binary, CommandBin/binary, Len:32/little, CRC/binary, Payload/binary>>.

atom_to_cmd(A) ->
    S = list_to_binary(atom_to_list(A)),
    L = byte_size(S),
    <<S/binary, 0:((12-L)*8)>>.

get_message(<<Hdr:16/binary, Len:32/little, CRC:4/binary,
                Payload:Len/binary, Rest/binary>>) ->
    case b_crypto:crc(Payload) of
            CRC -> {ok, <<Hdr/binary, Len:32/little, CRC/binary>>, Payload, Rest};
              _ -> {error, crc}
    end;

get_message(_B) ->
    {error, incomplete}.

%get_message(B) ->
%    util:senddump(B),
%    1 = 2.

cmd_to_list(B) ->
    L = binary_to_list(B),
    string:strip(L, right, 0).

parse_varint(<<16#fd, X:16/little, Rest/binary>>) -> {X, Rest};
parse_varint(<<16#fe, X:32/little, Rest/binary>>) -> {X, Rest};
parse_varint(<<16#ff, X:64/little, Rest/binary>>) -> {X, Rest};
parse_varint(<<X:8, Rest/binary>>) -> {X, Rest}.

varint(X) when X < 16#fd -> <<X>>;
varint(X) when X =< 16#ffff  -> <<16#fd, X:16/little>>;
varint(X) when X =< 16#ffffffff  -> <<16#fe, X:32/little>>;
varint(X) when X =< 16#ffffffffffffffff  -> <<16#ff, X:64/little>>.

parse_inv(B) ->
    {N, R} = parse_varint(B),
    {ok, N, [{T,H} || <<T:32/little, H:32/bytes>> <= R ]}.

parse_getdata(B) ->
    {N, R} = parse_varint(B),
    {ok, N, [{T,H} || <<T:32/little, H:32/bytes>> <= R ]}.

parse_addr(B) ->
    {N, R} = parse_varint(B),
    {ok, N, parse_addr(R, [])}.

parse_addr(<<_Time:32/little, _Services:8/binary, IPv6:16/binary, Port:16/big, Rest/binary>>, L) ->
    <<_:12/binary, IPv4_A:8, IPv4_B:8, IPv4_C:8, IPv4_D:8>> = IPv6,
    parse_addr(Rest, [{{IPv4_A, IPv4_B, IPv4_C, IPv4_D}, Port}|L]);

parse_addr(<<>>, L) ->
    L.

parse_header(<<Magic:4/binary, Command:12/binary, Len:32/little, CRC:4/binary>>) ->
    CommandAtom = case cmd_to_list(Command) of
        "version" -> version;
        "verack"  -> verack;
        "addr"    -> addr;
        "ping"    -> ping;
        "pong"    -> pong;
        "inv"     -> inv;
        "alert"   -> alert;
        "getdata" -> getdata;
           "tx"   -> tx;
             _    -> unknown
    end,
    {ok, Magic, CommandAtom, Len, CRC}.

parse_message(<<Magic:4/binary, Command:12/binary, Len:32/little, CRC:4/binary,
                Payload:Len/binary, Rest/binary>>) ->
    case b_crypto:crc(Payload) of
        CRC -> {ok, Magic, Command, CRC, Payload, Rest};
        _   -> {error, crc, Rest}
    end;
parse_message(Rest) ->
    {error, parse, Rest}.
 
getdata_msg(L) ->
    N = varint(length(L)),
    Body = <<<<Type:32/little, Hash/bytes>> || {Hash, Type} <- L>>,
    make_message(?MAGIC, getdata, <<N/bytes, Body/bytes>>). 
    
inv_msg(L) ->
    N = varint(length(L)),
    Body = <<<<Type:32/little, Hash/bytes>> || {Type, Hash} <- L>>,
    make_message(?MAGIC, inv, <<N/bytes, Body/bytes>>). 

getaddr_msg() ->
    make_message(?MAGIC, getaddr, <<>>).

tx_msg(B) ->
    make_message(?MAGIC, tx, B).

ping_msg() ->
    PayloadStr = [ "00 00 00 00 00 00 00 00" ],
    Payload = msgstr_to_binary(PayloadStr),
    make_message(?MAGIC, ping, Payload).

pong_msg(Payload) ->
    make_message(?MAGIC, pong, Payload).

addr_msg() ->
    {MegaS, S, _MicroS} = now(),
    T = MegaS*1000000 + S,
    make_message(?MAGIC, addr, 
        <<1, 
          T:32/little, 
          1, 0:(7*8),             %service
          0:(10*8), 16#FF, 16#FF, %IPv6 addr (first part)
          192, 210, 207, 147,      %IPv4 addr (or rest of IPv6 addr) 
          8333:16/big              %port
        >>).

verack_msg() ->
    make_message(?MAGIC, verack, <<>>).

version_msg() ->
    PayloadStr1 = [
               "62 EA 00 00",
               "01 00 00 00 00 00 00 00",
               "11 B2 D0 50 00 00 00 00",
               "01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF FF 00 00 00 00 00 00",
               "01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF FF 00 00 00 00 00 00"],
    PayloadStr2 = [
               "0D 2f 62 69 74 65 6e 3a 30 2e 30 2e 31 2f", % /biten:0.0.1/
               "C0 3E 03 00"
               ],
    Payload = list_to_binary([
        msgstr_to_binary(PayloadStr1),
        crypto:rand_bytes(8),
        msgstr_to_binary(PayloadStr2)
        ]),
    make_message(?MAGIC, version, Payload).

msgstr_to_binary(M) ->
    list_to_binary([hexstr_to_binary(X) || X <- M]).
