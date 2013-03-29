%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Common bitcoin crypto functions 
%%% @end
%%% --------------------------------------------------------------------------

-module(b_crypto).
-export([hash/1, crc/1]).

hash(Bin) ->
	erlsha2:sha256(erlsha2:sha256(Bin)).

crc(Bin) ->
	<<CRC:4/binary, _/binary>> = hash(Bin),
	CRC.
