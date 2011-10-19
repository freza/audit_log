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

-module(audit_log_i).
-vsn(' $Id: audit_log_i.erl 20123 2011-07-08 17:19:04Z jachym $ ').

-export([audit_logs/0, install/0, bootstrap/0]).

-import(error_logger, [info_msg/2]).
-import(lists, [member/2]).

-include("audit_log_db.hrl").

%%%

audit_logs() ->
    [audit_log, syslog].

bootstrap() ->
    mnesia:create_schema([node()]),
    mnesia:start().

install() ->
    mktab(audit_log_conf, [{attributes, record_info(fields, audit_log_conf)},
			   {type, set}]).

%%%

mktab(Tab, Opts) ->
    try mnesia:table_info(Tab, disc_copies) of
	Nodes ->
	    case member(node(), Nodes) of
		false ->
		    Res = mnesia:add_table_copy(Tab, node(), disc_copies),
		    info_msg("[Audit Log] Add ~s copy on ~s: ~p.~n", [Tab, node(), Res]);
		_ ->
		    ok
	    end
    catch
	exit : {aborted, {no_exists, _, _}} ->
	    Res = mnesia:create_table(Tab, [{disc_copies, [node()]} | Opts]),
	    info_msg("[Audit Log] Create table ~s: ~p.~n", [Tab, Res])
    end.
