%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Protocol related bits - message construction and parsing 
%%% @end
%%% --------------------------------------------------------------------------

-module(protocol).
-compile([export_all]).

-include("include/records.hrl").

-define(MAIN_MAGIC, <<16#F9BEB4D9:32/big>>).
-define(MAGIC, ?MAIN_MAGIC).
-define(PROTOCOL_VERSION, <<16#62EA0000:32/big>>).

hexstr_to_binary(S) ->
    T = string:tokens(S, " "),
    list_to_binary([list_to_integer(X, 16) || X <- T]).

binary_to_hexstr(B, D) ->
    util:bin_to_hex(B, D).

binary_to_hexstr(B) ->
    binary_to_hexstr(B, " ").

make_message(Magic, Command, Payload) ->
    Len = byte_size(Payload),
    CRC = b_crypto:crc(Payload),
    CommandBin = case Command of
        version -> atom_to_cmd(version);
        getaddr -> atom_to_cmd(getaddr);
        getdata -> atom_to_cmd(getdata);
        getheaders -> atom_to_cmd(getheaders);
         headers -> atom_to_cmd(headers);
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


format_pk_script(<<118, 169, 20, H160:20/bytes, 136, 172>>) ->
    io_lib:format("OP_DUP OP_HASH160 20 ~s OP_EQUALVERIFY OP_CHECKSIG", [util:base58_enc(0, H160)]);

format_pk_script(<<B/bytes>>) ->
    io_lib:format("~s", [binary_to_hexstr(B, "")]).

format_tx_in({H, I, Scr, Seq}) ->
    [io_lib:format("  tx_in~n    hash ~s~n    index ~b~n", [binary_to_hexstr(H, ""), I]),
     io_lib:format("    script ~s~n    seq ~b~n", [binary_to_hexstr(Scr), Seq])].

format_tx_out({Val, PK_script}) ->
    [io_lib:format("  tx_out~n    value ~b~n    pk_script ~s~n", [Val,
                            format_pk_script(PK_script)])].

format_tx(TX) ->
    [ io_lib:format("Transaction version ~b~n  tx_in_count = ~p~n", [
                        TX#tx.ver, TX#tx.tx_in_count]),
        [ format_tx_in(TX_in) || TX_in <- TX#tx.tx_in ],
        io_lib:format("  tx_out_count = ~b~n", [TX#tx.tx_out_count]),
        [ format_tx_out(TX_out) || TX_out <- TX#tx.tx_out ],
        io_lib:format("  lock time ~b~n", [TX#tx.lock_time]),
       "" 
    ].

parse_tx_in(Rest, R, 0) ->
    {lists:reverse(R), Rest};

parse_tx_in(<<TX_ref:32/bytes, Index:32/little, P/bytes>>, R, N) when N > 0 ->
    {L, R1} = parse_varint(P),
    <<Script:L/bytes, Seq:32/little, Rest/bytes>> = R1,
    parse_tx_in(Rest, [{TX_ref, Index, Script, Seq}|R], N - 1). 

parse_tx_out(Rest, R, 0) ->
    {lists:reverse(R), Rest};

parse_tx_out(<<Value:64/little, P/bytes>>, R, N) when N > 0 ->
    {L, R1} = parse_varint(P),
    <<PK_script:L/bytes, Rest/bytes>> = R1,
    parse_tx_out(Rest, [{Value, PK_script}|R], N - 1). 

parse_tx(<<Ver:32/little, B/bytes>>) ->
    {TX_in_count, R1} = parse_varint(B),
    {TX_in, R2} = parse_tx_in(R1, [], TX_in_count),
    {TX_out_count, R3} = parse_varint(R2),
    {TX_out, R4} = parse_tx_out(R3, [], TX_out_count),
    <<Lock_time:32/little>> = R4,
    #tx{ver = Ver, tx_in_count = TX_in_count, tx_in = TX_in, 
    tx_out_count = TX_out_count, tx_out = TX_out, lock_time = Lock_time}.

parse_headers(B) ->
    {N, R} = parse_varint(B),
    {ok, N, [H || <<H:80/bytes, _C:1/bytes>> <= R ]}.

parse_block_header(<<Ver:32/little, PrevBlock:32/bytes, MerkleRoot:32/bytes, Time:32/little, Diff:4/bytes, Nonce:32/little>>) -> 
    {Ver, PrevBlock,  MerkleRoot, Time, Diff, Nonce}.

prev_block_header(<<_:4/bytes, PrevBlock:32/bytes, _:44/bytes>>) -> 
    PrevBlock.

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
    Cmd = cmd_to_list(Command),
    CommandAtom = case lists:member(Cmd, [
                "version",
                "verack",
                "addr",
                "ping",
                "pong",
                "inv",
                "alert",
                "getdata",
                "getblocks",
                "getheaders",
                "headers",
                "notfound",
                "getaddr",
                "tx"
            ]) of
             true  -> list_to_atom(Cmd);
             false -> unknown
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
 
getheaders_msg(L, StopHash) ->
    N = varint(length(L)),
    HL = << <<Hash/bytes>> || Hash <- L >>,
    make_message(?MAGIC, getheaders, <<?PROTOCOL_VERSION/bytes, N/bytes, HL/bytes, StopHash/bytes>>).

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
