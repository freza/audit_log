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

-module(audit_log_sup).
-vsn(' $Id: audit_log_disk_sup.erl 20123 2011-07-08 17:19:04Z jachym $ ').
-behaviour(supervisor).

-export([start_link/0, add_worker/1, del_worker/1]).
-export([init/1]).

%%% Public interface.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_worker(Log) ->
    supervisor:start_child(?MODULE, child(Log)).

del_worker(Log) ->
    supervisor:terminate_child(?MODULE, Log),
    supervisor:delete_child(?MODULE, Log).

%%% Supervisor callbacks.

init([]) ->
    {ok, {{one_for_one, 10, 5000}, [child(Log) || Log <- mnesia:dirty_all_keys(audit_log_conf)]}}.

%%% Implementation.

child(Log) ->
    {Log, {audit_log_disk, start_link, [Log]}, transient, 60000, worker, [audit_log_disk]}.
