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

-module(audit_log_disk).
-vsn(' $Id: audit_log_disk.erl 20132 2011-07-11 15:55:17Z jachym $ ').
-behaviour(gen_server).

-export([start_link/1, send_msg/2, set_options/1, get_status/1, change_file/1, clean_old/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([async_send_msg/2]).

-import(audit_log_lib, [event_log/2, get_value/2, get_value/3, make_two/1, secs_to_midnight/0]).
-import(filename, [basename/1, dirname/1, rootname/2, absname/1, join/1, split/1]).

-include("audit_log_db.hrl").

%%% Public interface.

start_link(Log) ->
    %% gen_server:start_link(?MODULE, [Log], [{spawn_opt, [{fullsweep_after, 0}]}]).
    gen_server:start_link(?MODULE, [Log], []).

send_msg(Pid, Msg) ->
    gen_server:call(Pid, {send_msg, Msg}).

send_msg(Pid, Msg, Timeout) ->
    gen_server:call(Pid, {send_msg, Msg}, Timeout).

set_options(Pid) ->
    gen_server:call(Pid, set_options).

get_status(Pid) ->
    gen_server:call(Pid, get_status).

change_file(Pid) ->
    gen_server:call(Pid, change_file).

clean_old(Pid) ->
    gen_server:call(Pid, clean_old).

async_send_msg(Pid, Msg) ->
    gen_server:cast(Pid, {send_msg, Msg}).

%%% Generic server.

-record(state, {
	  log, 			%% Log name. 				:: atom()
	  path, 		%% Current file path. 			:: filename()
	  iod, 			%% Open file. 				:: io_device()
	  acc_size, 		%% Lines written so far. 		:: line_count()
	  change_tmr, 		%% File change timer. 			:: timer_ref()
	  change_ref, 		%% File change timer token. 		:: reference()
	  clean_tmr, 		%% Old file cleanup timer. 		::  timer_ref()
	  clean_ref, 		%% Old file cleanup timer token. 	:: reference()
	  dir, 			%% filename()
	  size_limit, 		%% line_count() | infinity
	  time_limit, 		%% hours()
	  lifetime, 		%% days()
	  cache_size, 		%% kilobytes()
	  cache_time 		%% msec()
	 }).

init([Log]) ->
    %% The only reason for trap_exit is that we always want to close & rename logfile cleanly.
    process_flag(trap_exit, true),
    case configure(default(Log)) of
	{#state{dir = Dir} = State, _} when is_list(Dir) ->
	    State2 = #state{path = Path} = open(State),
	    proc_lib:spawn_link(fun () -> clean_open(Log, dirname(Path), Path) end),
	    Timer = schedule_clean_old(Ref = make_ref()),
	    audit_log_ctrl ! {worker_up, Log, self()},
	    {ok, State2#state{clean_tmr = Timer, clean_ref = Ref}};
	_ ->
	    exit(no_directory)
    end.

handle_call({send_msg, Msg}, _, State0) ->
    case write(State0, Msg) of
	#state{acc_size = 0} = State ->
	    {reply, changed_file, State};
	State ->
	    {reply, ok, State}
    end;
handle_call(set_options, _, State) ->
    State2 = case configure(State) of
		 {StateX, true} ->
		     change(StateX);
		 {StateX, _} ->
		     StateX
	     end,
    {reply, ok, State2};
handle_call(get_status, _, #state{path = Path, acc_size = AS} = State) ->
    {reply, {online, AS, Path}, State};
handle_call(change_file, _, State) ->
    {reply, ok, change(State)};
handle_call(clean_old, _, #state{clean_tmr = Timer} = State) ->
    erlang:cancel_timer(Timer),
    {reply, ok, cleanup(State)};
handle_call(_, _, State) ->
    {reply, bad_request, State}.

handle_info({confirm_up, From}, State) ->
    gen_server:reply(From, ok),
    {noreply, State};
handle_info({clean_old, CR}, #state{clean_ref = CR} = State) ->
    {noreply, cleanup(State)};
handle_info({change, CR}, #state{change_ref = CR} = State) ->
    {noreply, change(State)};
handle_info({'EXIT', _, Reason}, State) when Reason /= normal ->
    {stop, Reason, State};
handle_info(_, State) ->
    {noreply, State}.

handle_cast({send_msg, Msg}, State) ->
    {noreply, write(State, Msg)};
handle_cast(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, State) ->
    close(State).

%%% Implementation.

cleanup(#state{log = Log, path = Path, lifetime = LT} = State) ->
    proc_lib:spawn_link(fun () -> clean_old(Log, dirname(Path), LT) end),
    Timer = schedule_clean_old(Ref = make_ref()),
    State#state{clean_tmr = Timer, clean_ref = Ref}.

write(#state{iod = Iod, acc_size = Size} = State, Msg) ->
    case file:write(Iod, Msg) of
	ok ->
	    check(State#state{acc_size = Size + 1});
	{error, _} = Err ->
	    exit({file_write, Err})
    end.

check(#state{acc_size = Acc, size_limit = Max} = State) when is_integer(Max), Acc >= Max ->
    change(State);
check(State) ->
    State.

change(#state{change_tmr = Timer} = State) ->
    erlang:cancel_timer(Timer),
    open(close(State#state{change_tmr = nil, change_ref = nil})).

open(#state{log = Log, dir = Dir, time_limit = TL, iod = nil, cache_size = CS, cache_time = CT} = S) ->
    {ok, Iod} = file:open(Path = logfile(Log, Dir), cache_opts([raw, write, binary], CS, CT)),
    Timer = schedule_file_change(TL, CR = make_ref()),
    event_log("~s,open_file,~s.~n", [Log, Path]),
    S#state{iod = Iod, path = Path, change_tmr = Timer, change_ref = CR}.

cache_opts(Opts, CS, CT) when CS == 0; CT == 0 ->
    Opts;
cache_opts(Opts, CS, CT) ->
    [{delayed_write, CS * 1024, CT} | Opts].

close(#state{iod = nil} = State) ->
    State;
close(#state{log = Log, iod = Iod, path = Path, acc_size = Size} = State) ->
    ok = file:close(Iod),
    case Size == 0 of
	true ->
	    case file:delete(Path) of
		ok ->
		    ok;
		{error, enoent} ->
		    ok;
		{error, _} = Err ->
		    exit({delete_file, Err})
	    end;
	_ ->
	    case file:rename(Path, rootname(Path, ".open")) of
		ok ->
		    ok;
		{error, _} = Err ->
		    exit({rename_file, Err})
	    end
    end,
    event_log("~s,close_file,~s.~n", [Log, Path]),
    State#state{iod = nil, path = nil, acc_size = 0}.

clean_open(Log, Dir, Cur) ->
    Strip = fun (F, N) when F /= Cur ->
		    file:rename(F, rootname(F, ".audit.open")),
		    N + 1;
		(_, N) ->
		    N
	    end,
    N = filelib:fold_files(Dir, regexp(Log, ".audit.open"), false, Strip, 0),
    event_log("~s,clean_open,~w.~n", [Log, N]).

clean_old(Log, Dir, Limit) ->
    LT = erlang:localtime(),
    Clean = fun (F, N) ->
		    DT = filedate(basename(F) -- log_to_list(Log) -- "_"),
		    case calendar:time_difference(DT, LT) of
			{D, _} when D >= Limit ->
			    ok = file:delete(F),
			    N + 1;
			_ ->
			    N
		    end
	    end,
    N = filelib:fold_files(Dir, regexp(Log, ".audit"), false, Clean, 0),
    event_log("~s,clean_old,~w.~n", [Log, N]).

%%% Utilities.

default(Log) ->
    #state{log = Log, path = nil, iod = nil, change_tmr = nil, acc_size = 0, dir = nil}.

configure(#state{log = Log, cache_size = CS0, cache_time = CT0, size_limit = SL0, time_limit = TL0, dir = LD0,
		 lifetime = LT0} = State) ->
    [#audit_log_conf{opts = Opts}] = mnesia:dirty_read(audit_log_conf, Log),
    CS = get_value(cache_size, Opts, CS0),
    CT = get_value(cache_time, Opts, CT0),
    SL = get_value(size_limit, Opts, SL0),
    TL = get_value(time_limit, Opts, TL0),
    LT = get_value(lifetime, Opts, LT0),
    LD = get_value(dir, Opts, LD0),
    Change = (SL /= SL0) or (TL /= TL0),
    State2 = State#state{cache_size = CS, cache_time = CT, size_limit = SL, time_limit = TL,
			 lifetime = LT, dir = absname(LD)},
    {State2, Change}.

logfile(Log, Dir) ->
    logfile(Log, Dir, nil).

logfile(Log, Dir, N) ->
    Timestamp = file_ts(erlang:localtime()),
    if is_atom(N) ->
	    Seq = "";
       true ->
	    Seq = [".", make_two(N)]
    end,
    mkdir(dirname(Path = join([Dir, [log_to_list(Log), "_", Timestamp, Seq, ".audit"]]))),
    case filelib:is_file(Path) of
	true ->
	    logfile(Log, Dir, if is_atom(N) -> 1; true -> N + 1 end);
	_ ->
	    Path ++ ".open"
    end.

mkdir(Path) ->
    mkdir(split(Path), []).

mkdir([Dir | Tail], Parent) ->
    case file:make_dir(Path = join([Parent, Dir])) of
	ok ->
	    ok;
	{error, eexist} ->
	    ok
    end,
    mkdir(Tail, Path);
mkdir([], _) ->
    ok.

schedule_clean_old(Ref) ->
    erlang:send_after(1000 * secs_to_midnight(), self(), {clean_old, Ref}).

schedule_file_change(Hours, Ref) ->
    erlang:send_after(interval(erlang:now(), Hours), self(), {change, Ref}).

interval(Now, Hours) when is_integer(Hours), Hours > 0 ->
    T1 = calendar:now_to_local_time(Now),
    T3 = round(calendar:now_to_local_time(increment(Now, Hours)), Hours),
    {D, {H, M, S}} = calendar:time_difference(T1, T3),
    ((D * 86400) + (H * 3600) + (M * 60) + S) * 1000.

increment({MS, S, _}, Hours) ->
    Seconds = S + (Hours * 3600),
    {MS + (Seconds div 1000000), (Seconds rem 1000000), 0}.

round({Date, {_, _, _}}, Hours) when (Hours rem 24) == 0 ->
    {Date, {0, 0, 0}};
round({Date, {H, _, _}}, _) ->
    {Date, {H, 0, 0}}.

file_ts({{YY, MM, DD}, {Hh, Mm, Ss}}) ->
    [integer_to_list(YY), make_two(MM), make_two(DD), make_two(Hh), make_two(Mm), make_two(Ss)].

regexp(Log, Suf) ->
    [log_to_list(Log), $_,
     "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]",
     ".[0-9][0-9]" | Suf].

log_to_list(syslog) ->
    [App, Node] = string:tokens(atom_to_list(node()), "@"),
    "syslog_" ++ App ++ "_" ++ Node;
log_to_list(Log) ->
    atom_to_list(Log).

filedate([Y1, Y2, Y3, Y4, $-, M1, M2, $-, D1, D2, $_, H1, H2, $-, M3, M4, $-, S1, S2 | _]) ->
    {{dec(Y1, Y2, Y3, Y4), dec(M1, M2), dec(D1, D2)}, {dec(H1, H2), dec(M3, M4), dec(S1, S2)}}.

dec(N1, N2, N3, N4) ->
    (dig(N1) * 1000) + (dig(N2) * 100) + (dig(N3) * 10) + dig(N4).

dec(N1, N2) ->
    (dig(N1) * 10) + dig(N2).

dig(N) when is_integer(N), N >= $0, N =< $9 ->
    N - $0.
