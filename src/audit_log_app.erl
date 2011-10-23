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

-module(audit_log_app).
-vsn(' $Id: audit_log_app.erl 20123 2011-07-08 17:19:04Z jachym $ ').
-behaviour(application).

-export([start/2, stop/1]).

-import(lists, [member/2]).

-include("audit_log_db.hrl").

%%% Application.

start(normal, _) ->
    setup_db(),
    {ok, _} = audit_log:open_log(syslog),
    audit_log_sup:start_link().

stop(_) ->
    ok.

%%% Implementation.

setup_db() ->
    try mnesia:table_info(audit_log_conf, disc_copies) of
	Nodes ->
	    case member(node(), Nodes) of
		false ->
		    {atomic, ok} = mnesia:add_table_copy(audit_log_conf, node(), disc_copies);
		_ ->
		    ok
	    end
    catch
	exit : {aborted, {no_exists, _, _}} ->
	    {atomic, ok} = audit_log:create_db()
    end,
    ok = mnesia:wait_for_tables([audit_log_conf], 10000).
