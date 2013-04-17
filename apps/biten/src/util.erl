%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Helper functions.
%%% @end
%%% --------------------------------------------------------------------------

-module(util).
-export([print_tx_file/1, hexdump/1, hex_to_bin/1, to_base58/1, base58_enc/2, ip_to_str/1, crc_to_str/1, senddump/1, startdump/0, dump/0, strftime/1]).
-export([take_random/2, take_random/3, random_element/1, random_element/2]).

hexdump(B) -> hexdump(B, 0).

hexdump(<<>>, N) ->
    if N rem 16 /= 15 -> io:format("~n");
                 true -> true
    end,
    ok;
hexdump(<<H:8, Rest/binary>>, N) ->
    R = N rem 16,
    if R == 0 -> io:format("~8.16.0b ", [N]);
         true -> true
    end,
    io:format("~2.16.0b", [H]),
    case R of
       7  -> io:format("  ");
       15 -> io:format("~n");
       _  -> io:format(" ")
    end,
    hexdump(Rest, N + 1).

ip_to_str(undefined) ->
    "undefined";

ip_to_str({A, B, C, D}) ->
    io_lib:format("~B.~B.~B.~B", [A,B,C,D]).

crc_to_str(<<CRC:32/big>>) ->
    io_lib:format("~8.16.0b", [CRC]).

senddump(B) ->
    dump ! B,
    ok.

startdump() ->
    register(dump, spawn_link(?MODULE, dump, [])).

dump() ->
    receive
        M -> hexdump(M)
    end,
    dump().

strftime({{Y, M, D}, {H, Min, Sec}}) ->
    io_lib:format("~4..0b.~2..0b.~2..0b ~2..0b:~2..0b:~2..0b", [Y, M, D, H, Min, Sec]).

leading_zeros(B) ->
    leading_zeros(B, 0).

leading_zeros(<<0, R/bytes>>, X) ->
    leading_zeros(R, X + 1);

leading_zeros(R, X) when is_binary(R) ->
    X.

-define(CODE_ARRAY, array:from_list("123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")).

code(X) when X < 58 ->
    array:get(X, ?CODE_ARRAY).

to_base58(B) ->
    N = size(B)*8,
    <<X:N/big>> = B,
    Z = leading_zeros(B),
    prepend(to_base58(X, []), $1, Z).

to_base58(0, L)  ->
    L;

to_base58(N, L) ->
    to_base58(N div 58, [code(N rem 58)|L]).

prepend(L, S, N) when N > 0 ->
    prepend([S|L], S, N-1);

prepend(L, _, 0) ->
    L.

base58_enc(Ver, B) ->
    S1 = <<Ver, B/bytes>>,
    S2 = b_crypto:crc(S1),
    S3 = <<S1/bytes, S2/bytes>>,
    to_base58(S3).

digit(X) when X >= $0, X =< $9 ->
    X - $0;

digit(X) when X >= $a, X =< $z ->
    X - $a + 10;

digit(X) when X >= $A, X =< $Z ->
    X - $A + 10.

hex_to_bin(S) ->
    binary:list_to_bin(hex_to_bin(S, [])).

hex_to_bin([], R) ->
    lists:reverse(R); 

hex_to_bin([$\  | T], R) ->
    hex_to_bin(T, R);

hex_to_bin([A, B | T], R) ->
    hex_to_bin(T, [digit(A)*16+digit(B)|R]).

print_tx_file(F) ->
    {ok, TX_raw} = file:read_file(F),
    {_, _, TX, _} = protocol:get_message(TX_raw),
    io:format("~s", [protocol:format_tx(protocol:parse_tx(TX))]).

%% take at most N random elements from list
%% runtime proportional to length of the list
take_random(N, List) ->
    take_random(N, List, length(List)).

take_random(N, List, Len) ->
    take_random(N, List, Len, []).

take_random(_, [], _, Acc) ->
    Acc;

take_random(N, L, Len, Acc) when N >= Len ->
    Acc ++ L;

take_random(0, _, _, Acc) ->
    Acc;

take_random(1, L, Len, Acc) ->
    [random_element(L, Len) | Acc];

take_random(N, [H | T], Len, Acc) ->
    R = random:uniform(Len),
    case N >= R of
        true ->
            take_random(N-1, T, Len-1, [H | Acc]);
        false ->
            take_random(N, T, Len-1, Acc)
    end.

random_element(List) ->
    random_element(List, length(List)).

random_element(List, Len) ->
    R = random:uniform(Len),
    lists:nth(R, List).
