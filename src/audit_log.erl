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

-module(audit_log).
-vsn(' $Id: audit_log.erl 20132 2011-07-11 15:55:17Z jachym $ ').

-export([open_log/1, open_log/2, audit_msg/2, audit_msg/3, syslog_msg/1, syslog_msg/2, close_log/1]).
-export([get_config/0, get_config/1, set_config/1, set_config/2, get_status/0, get_status/1]).
-export([change_file/1, clean_old/1, rediscover_logs/0]).
-export([create_db/0, create_db/1]).

-import(audit_log_lib, [get_env/3]).
-import(lists, [foreach/2, keysort/2]).

-include("audit_log_db.hrl").

%%% User API.

syslog_msg(Msg) ->
    audit_msg(syslog, Msg).

syslog_msg(Fmt, Args) ->
    audit_msg(syslog, Fmt, Args).

audit_msg(Log, Msg) ->
    audit_log_disk:send_msg(Log, Msg).

audit_msg(Log, Fmt, Args) ->
    audit_log_disk:send_msg(Log, io_lib:format(Fmt, Args)).

%%% Management API.

open_log(Log) ->
    open_log(Log, []).

open_log(Log, Opts) ->
    mnesia:activity(transaction, fun () -> do_set_config(Log, Opts) end),
    try audit_log_sup:add_worker(Log) of
	{ok, _} ->
	    {ok, started};
	{error, {already_started, _}} ->
	    {ok, running};
	{error, _} = Err ->
	    Err
    catch
	exit : {noproc, _} ->
	    {ok, added}
    end.

close_log(Log) ->
    try audit_log_sup:del_worker(Log) catch
	exit : {noproc, _} ->
	    ok
    end,
    mnesia:activity(transaction, fun () -> mnesia:delete(audit_log_conf, Log, write) end).

change_file(Log) ->
    audit_log_disk:change_file(Log).

clean_old(Log) ->
    audit_log_disk:clean_old(Log).

rediscover_logs() ->
    %% This is provided just for peace of mind but isn't really believed to be useful.
    [{Log, open_log(Log)} || Log <- mnesia:dirty_all_keys(audit_log_conf)].

%%% Maintenance API.

get_config() ->
    Read = fun () ->
		   [{Log, do_get_config(Log)} || Log <- mnesia:all_keys(audit_log_conf)]
	   end,
    keysort(1, mnesia:activity(transaction, Read)).

get_config(Log) ->
    mnesia:activity(transaction, fun () -> do_get_config(Log) end).

set_config(Opts) ->
    Edit = fun () ->
		   foreach(fun (Log) -> do_set_config(Log, Opts) end, All = mnesia:all_keys(audit_log_conf)),
		   All
	   end,
    [(catch audit_log_disk:set_options(Log)) || Log <- mnesia:activity(transaction, Edit)],
    ok.

set_config(Log, Opts) ->
    mnesia:activity(transaction, fun () -> do_set_config(Log, Opts) end),
    audit_log_disk:set_options(Log).

get_status() ->
    keysort(1, [{Log, get_status(Log)} || Log <-  mnesia:dirty_all_keys(audit_log_conf)]).

get_status(Log) ->
    try
	audit_log_disk:get_status(Log)
    catch
	exit : {noproc, _} ->
	    down
    end.

%%% System API. Normally not needed, but allow creating configuration table manually.

create_db() ->
    create_db([{disc_copies, [node()]}, {local_content, true}]).

create_db(Opts) ->
    mnesia:create_table(audit_log_conf, [{attributes, record_info(fields, audit_log_conf)},
					 {type, set} | Opts]).

%%% Utilities.

do_get_config(Log) ->
    [#audit_log_conf{opts = Opts}] = mnesia:read(audit_log_conf, Log),
    Opts.

do_set_config(Log, Opts) ->
    case mnesia:read(audit_log_conf, Log) of
	[#audit_log_conf{opts = Prev} = CF] ->
	    mnesia:write(CF#audit_log_conf{log = Log, opts = Os = merge_opts(Opts, Prev)}),
	    Os;
	[] ->
	    mnesia:write(#audit_log_conf{log = Log, opts = Os = merge_opts(Opts, default_opts())}),
	    Os
    end.

default_opts() ->
    [{cache_size, 128}, {cache_time, 1000}, {size_limit, 200000}, {time_limit, 24}, {lifetime, 7},
     {dir, default_dir()}].

default_dir() ->
    get_env(audit_log, default_dir, filename:join([code:root_dir(), "audit_logs"])).

merge_opts(NL, OL) ->
    orddict:merge(fun (_, NV, _) -> NV end, orddict:from_list(NL), orddict:from_list(OL)).
