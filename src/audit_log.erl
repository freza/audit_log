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

-import(audit_log_lib, [apply_pid/2, do_set_config/2]).
-import(lists, [foreach/2, keysort/2, map/2]).

-include("audit_log_db.hrl").

%%% User API.

syslog_msg(Msg) ->
    audit_msg(syslog, Msg).

syslog_msg(Msg, Timeout) ->
    audit_msg(syslog, Msg, Timeout).

audit_msg(Log, Msg) ->
    apply_pid(Log, fun (Pid) -> audit_log_disk:send_msg(Pid, Msg) end).

audit_msg(Log, Msg, Timeout) ->
    apply_pid(Log, fun (Pid) -> audit_log_disk:send_msg(Pid, Msg, Timeout) end).

%%% Management API.

open_log(Log) ->
    open_log(Log, []).

open_log(Log, Opts) ->
    audit_log_ctrl:open_log(Log, Opts).

close_log(Log) ->
    audit_log_ctrl:close_log(Log).

change_file(Log) ->
    apply_pid(Log, fun audit_log_disk:change_file/1).

clean_old(Log) ->
    apply_pid(Log, fun audit_log_disk:clean_old/1).

rediscover_logs() ->
    audit_log_ctrl:rediscover_logs().

%%% Maintenance API.

get_config() ->
    Read = fun () ->
		   map(fun (Log) -> {Log, do_get_config(Log)} end, mnesia:all_keys(audit_log_conf))
	   end,
    keysort(1, mnesia:activity(transaction, Read)).

get_config(Log) ->
    mnesia:activity(transaction, fun () -> do_get_config(Log) end).

set_config(Opts) ->
    %% This could benefit from issuing update concurrently, but OTOH nobody cares.
    Edit = fun () ->
		   foreach(fun (Log) -> do_set_config(Log, Opts) end, All = mnesia:all_keys(audit_log_conf)),
		   All
	   end,
    foreach(fun (Log) -> catch apply_pid(Log, fun audit_log_disk:set_options/1) end, mnesia:activity(transaction, Edit)).

set_config(Log, Opts) ->
    mnesia:activity(transaction, fun () -> do_set_config(Log, Opts) end),
    apply_pid(Log, fun audit_log_disk:set_options/1).

get_status() ->
    keysort(1, map(fun (Log) -> do_get_status(Log) end, mnesia:dirty_all_keys(audit_log_conf))).

get_status(Log) ->
    apply_pid(Log, fun (Pid) -> audit_log_disk:get_status(Pid) end).

%%% Utilities.

do_get_status(Log) ->
    try
	{Log, apply_pid(Log, fun (Pid) -> audit_log_disk:get_status(Pid) end)}
    catch
	exit : {log_missing, _} ->
	    {Log, {not_started, nil, nil}};
	exit : {noproc, _} ->
	    {Log, {not_available, nil, nil}}
    end.

do_get_config(Log) ->
    [#audit_log_conf{opts = Opts}] = mnesia:read(audit_log_conf, Log),
    Opts.
