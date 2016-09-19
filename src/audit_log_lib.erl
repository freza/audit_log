%%% Copyright (c) 2012 Jachym Holecek <freza@circlewave.net>
%%% Copyright (c) 2005-2011 Everything Everywhere Ltd.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.

-module(audit_log_lib).
-vsn(' $Id: audit_log_lib.erl 20123 2011-07-08 17:19:04Z jachym $ ').

-export([get_value/2, get_value/3]).
-export([make_three/1, make_two/1]).
-export([secs_to_midnight/0, secs_to_midnight/1]).
-export([printable_date/0, printable_date/1, plain_ts/0, plain_ts/1]).
-export([get_env/3]).

-include("audit_log_db.hrl").

%%%

get_value(K, L) ->
    case lists:keysearch(K, 1, L) of
	{value, {_, V}} ->
	    V;
	false ->
	    exit({key_missing, K})
    end.

get_value(K, L, D) ->
    case lists:keysearch(K, 1, L) of
	{value, {_, V}} ->
	    V;
	false ->
	    D
    end.

make_two(N) when N >= 0, N < 10 ->
    [$0 | integer_to_list(N)];
make_two(N) when N >= 0, N < 100 ->
    integer_to_list(N).

make_three(N) when N >= 0, N < 10 ->
    [$0, $0 | integer_to_list(N)];
make_three(N) when N >= 0, N < 100 ->
    [$0 | integer_to_list(N)];
make_three(N) when N >= 0, N < 1000 ->
    integer_to_list(N).

secs_to_midnight() ->
    secs_to_midnight(calendar:local_time()).

secs_to_midnight({_, {H, M, S}}) ->
    86400 - (H*3600 + M*60 + S).

plain_ts() ->
    plain_ts(erlang:localtime()).

plain_ts({_, _, _} = Now) ->
    plain_ts(calendar:now_to_local_time(Now));
plain_ts({{YY, MM, DD}, {Hh, Mm, Ss}}) ->
    [integer_to_list(YY), make_two(MM), make_two(DD), make_two(Hh), make_two(Mm), make_two(Ss)].

printable_date() ->
    printable_date(os:timestamp()).

printable_date({_, _, MicroSecs} = Now) ->
    [printable_date(calendar:now_to_local_time(Now)), $., make_three(MicroSecs div 1000)];
printable_date({{Y, Mo, D}, {H, M, S}}) ->
    [integer_to_list(Y), $-, make_two(Mo), $-, make_two(D), $_, make_two(H), $:, make_two(M), $:, make_two(S)].

get_env(App, Key, Def) ->
    case application:get_env(App, Key) of
        {ok, Val} ->
            Val;
	undefined ->
            Def
    end.
