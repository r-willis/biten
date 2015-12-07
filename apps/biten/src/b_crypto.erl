%%% --------------------------------------------------------------------------
%%% @author Randy Willis <willis.randy@ymail.com
%%% @doc Common bitcoin crypto functions 
%%% @end
%%% --------------------------------------------------------------------------

-module(b_crypto).
-export([hash/1, crc/1]).
-compile(export_all).

hash(Bin) ->
	crypto:hash(sha256, crypto:hash(sha256, Bin)).

crc(Bin) ->
	<<CRC:4/binary, _/binary>> = hash(Bin),
	CRC.

%% EC secp256k1 curve

%% Prime field order
-define(P, 16#fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f).
%-define(P, 29).
%% Curve coefficient
-define(A, 0).
%-define(A, 4).
%% Curve coefficient
-define(B, 7).
%-define(B, 20).
%% Base point
-define(G, {16#79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798, 16#483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8}).
%% Curve order
-define(N, 16#fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141).
%% Cofactor
-define(H, 1).

%% Prime field arithmetic
pf_add(X, Y) ->
    (X + Y) rem ?P.

pf_sub(X, Y) ->
    (X - Y + ?P) rem ?P.

pf_neg(X) ->
    ?P - X.

pf_mul(X, Y) ->
    (X*Y) rem ?P.

%% Inversion using the extended Euclidean algorithm
%% Algorithm 2.20 from "Guide to Elliptic Cruve Cryptography"
%% inefficient, but simple
pf_inv(X) ->
    pf_inv(X, ?P).

pf_inv(X, P) ->
    (pf_inv(X, P, 1, 0) + P) rem P.

pf_inv(U, _V, X1, _X2) when U =:= 1 ->
    X1;
     
pf_inv(U, V, X1, X2) when U > 1 ->
    Q = V div U,
    pf_inv(V - Q*U, U, X2 - Q*X1, X1).

%% Elliptic curve arithmetic
ec_add(X, infinity) ->
    X;

ec_add(infinity, X) ->
    X;

%% Negatives
ec_add({X, Y1}, {X, Y2}) when Y1 + Y2 =:= ?P ->
    infinity;

%% Point doubling
ec_add({X, Y}, {X, Y}) ->
    T1 = pf_inv(pf_mul(2, Y)),
    T2 = pf_add(pf_mul(3, pf_mul(X, X)), ?A),
    T3 = pf_mul(T2, T1),
    T4 = pf_mul(T3, T3),
    X3 = pf_sub(T4, pf_mul(X, 2)),
    T5 = pf_sub(X, X3),
    T6 = pf_mul(T3, T5),
    Y3 = pf_sub(T6, Y),
    {X3, Y3};

%% Point addition
ec_add({X1, Y1}, {X2, Y2}) ->
    DX = pf_sub(X2, X1),
    DY = pf_sub(Y2, Y1),
    DXI = pf_inv(DX),
    T1 = pf_mul(DY, DXI),
    T2 = pf_mul(T1, T1),
    X3 = pf_sub(pf_sub(T2, X1), X2),
    T4 = pf_mul(T1, pf_sub(X1, X3)),
    Y3 = pf_sub(T4, Y1),
    {X3, Y3}.

ec_neg({X, Y}) ->
    {X, pf_neg(Y)}.

ec_pmul(X, P) ->
    ec_pmul(X, P, infinity).

ec_pmul(0, _, Q) ->
    Q;

ec_pmul(X, P, Q) ->
    B = X band 1,
    Q1 = case B of
        1 ->
            ec_add(Q, P);
        0 ->
            Q
    end,
    ec_pmul(X bsr 1, ec_add(P, P), Q1). 

p() ->
    ?P.

g() ->
    ?G.

n() ->
    ?N.

check_key(_) ->
    true.

gen_kr() ->
    K = random:uniform(?N - 1),
    {X1, _} = ec_pmul(K, ?G),
    R = X1 rem ?N,
    case R of
        0 ->
            gen_kr();
        _ ->
            {K, R}
    end.

gen_sig(PrivKey, Z) ->
    {K, R} = gen_kr(),
    S = (pf_inv(K, ?N)*(Z + R*PrivKey)) rem ?N,
    case S of
        0 ->
            gen_sig(PrivKey, Z);
        _ ->
            {R, S}
    end.

check_sig(PubKey, {R, S}, Z) when R >= 1, R =< ?N - 1, S >= 1, S =< ?N - 1 ->
    case check_key(PubKey) of
        true ->
            ok;
        false ->
            throw({error, badkey})
    end,
    W = pf_inv(S, ?N),
    U1 = (Z*W) rem ?N,
    U2 = (R*W) rem ?N,
    {X1, _} = ec_add(ec_pmul(U1, ?G), ec_pmul(U2, PubKey)),
    R =:= X1 rem ?N.
