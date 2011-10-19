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

-module(audit_log_ctrl).
-vsn(' $Id: audit_log_ctrl.erl 20123 2011-07-08 17:19:04Z jachym $ ').
-behaviour(gen_server).

-export([start_link/0, open_log/2, close_log/1, rediscover_logs/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(audit_log_lib, [event_log/2, get_env/3]).
-import(lists, [filter/2]).

-include("audit_log_db.hrl").

%%% Public interface.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

open_log(Log, Opts) ->
    gen_server:call(?MODULE, {open_log, Log, Opts}, 5000).

close_log(Log) ->
    gen_server:call(?MODULE, {close_log, Log}, 60000).

rediscover_logs() ->
    gen_server:call(?MODULE, rediscover_logs, 60000).

%%% Generic server.

-record(state, {
	  discover_tmr, 		%% timer_ref()
	  discover_ref 			%% reference()
	 }).

init([]) ->
    ets:new(audit_log_ctrl, [public, set, named_table]),
    Timer = schedule_rediscover_logs(Ref = make_ref()),
    {ok, #state{discover_tmr = Timer, discover_ref = Ref}}.

handle_call(rediscover_logs, _, #state{discover_tmr = DT} = State) ->
    erlang:cancel_timer(DT),
    {reply, ok, rediscover(State)};
handle_call({open_log, Log, Opts}, From, State) ->
    case create(Log, Opts) of
	Pid when is_pid(Pid) ->
	    try
	        %% Makes sure log is operational right after open_log/N returns 'ok'.
		Pid ! {confirm_up, From},
		{noreply, State}
	    catch
		error : badarg ->
		    {reply, {error, not_running}, State}
	    end;
	exists ->
	    {reply, ok, State}
    end;
handle_call({close_log, Log}, _, State) ->
    {reply, close(Log), State};
handle_call(_, _, State) ->
    {reply, bad_request, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({worker_up, Log, Pid}, State) ->
    erlang:monitor(process, Pid),
    ets:insert(audit_log_ctrl, {Log, Pid}),
    {noreply, State};
handle_info({'DOWN', _, process, Pid, _}, State) when is_pid(Pid) ->
    ets:select_delete(audit_log_ctrl, [{{'_', Pid}, [], [true]}]),
    {noreply, State};
handle_info({rediscover_logs, RR}, #state{discover_ref = RR} = State) ->
    {noreply, rediscover(State)};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%% Implementation.

rediscover(State) ->
    Logs = mnesia:activity(transaction, fun () -> mnesia:all_keys(audit_log_conf) end),
    New = filter(fun (Log) -> case create(Log, []) of exists -> false; _ -> true end end, app_logs(Logs)),
    event_log("rediscover,~1000p.~n", [New]),
    Timer = schedule_rediscover_logs(Ref = make_ref()),
    State#state{discover_tmr = Timer, discover_ref = Ref}.

schedule_rediscover_logs(Ref) ->
    erlang:send_after(get_env(audit_log, discover_period, 300) * 1000, self(), {rediscover_logs, Ref}).

app_logs(Acc) ->
    app_logs(application:loaded_applications(), Acc).

app_logs([{App, _, _} | Apps], Acc) ->
    New = try
              Mod = list_to_existing_atom(atom_to_list(App) ++ "_i"),
              case Mod:audit_logs() of
                  Logs when is_list(Logs) ->
                      [X || X <- Logs, is_atom(X)];
                  _ ->
                      []
              end
          catch
              error : badarg ->	%% list_to_existing_atom()
                  [];
              error : undef -> 	%% Mod:audit_logs()
                  []
          end,
    app_logs(Apps, New ++ Acc);
app_logs([], Acc) ->
    lists:usort(Acc).

create(Log, Ini) ->
    case ets:lookup(audit_log_ctrl, Log) of
	[{_, _}] ->
	    exists;
	[] ->
	    options(Log, Ini),
	    case audit_log_disk_sup:add_worker(Log) of
		{ok, Pid} ->
		    Pid;
		{error, {already_started, _}} ->
		    exists
	    end
    end.

options(Log, Ini) ->
    mnesia:activity(transaction, fun () -> audit_log_lib:do_set_config(Log, Ini) end).

close(Log) ->
    case ets:lookup(audit_log_ctrl, Log) of
	[{_, _}] ->
	    ets:delete(audit_log_ctrl, Log),
	    audit_log_disk_sup:del_worker(Log),
	    ok;
	[] ->
	    ok
    end.
