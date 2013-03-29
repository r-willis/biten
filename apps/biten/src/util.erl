%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Helper functions.
%%% @end
%%% --------------------------------------------------------------------------

-module(util).
-export([hexdump/1, ip_to_str/1, crc_to_str/1, senddump/1, startdump/0, dump/0, strftime/1]).

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
