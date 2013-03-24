BitEN
=====

Bitcoin Erlang Node - scalable bitcoin daemon

This program is licensed under AGPL v3 or later (https://www.gnu.org/licenses/agpl-3.0.html).
Author Randy Willis <willis.randy@ymail.com
Module sha2.erl is licensed under BSD license and written by Stephen B. Vinoski.

1. Depedencies.

To run this program, you must have erlang and rebar installed.


2. Compilation.

[biten]$ rebar compile


3. Running.

[biten]$ erl -pa ebin
Erlang R14B04 (erts-5.8.5) [source] [smp:8:4] [rq:8] [async-threads:0] [kernel-poll:false]

Eshell V5.8.5  (abort with ^G)
1> application:start(sasl).
<skip>
2> application:start(biten).
<skip>
3> stat:print().
