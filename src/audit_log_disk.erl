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

-module(audit_log_disk).
-vsn(' $Id: audit_log_disk.erl 20132 2011-07-11 15:55:17Z jachym $ ').
-behaviour(gen_server).

-export([start_link/1, send_msg/2, set_options/1, get_status/1, change_file/1, clean_old/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(audit_log_lib, [get_value/3, make_two/1, secs_to_midnight/0]).
-import(filename, [basename/1, dirname/1, rootname/2, absname/1, join/1, split/1]).
-import(lists, [flatten/1]).

-include("audit_log_db.hrl").

%%% Public interface.

start_link(Log) ->
    %% gen_server:start_link({local, Log}, ?MODULE, [Log], [{spawn_opt, [{fullsweep_after, 0}]}]).
    gen_server:start_link({local, Log}, ?MODULE, [Log], []).

send_msg(Pid, Msg) ->
    gen_server:call(Pid, {send_msg, Msg}).

set_options(Pid) ->
    gen_server:call(Pid, set_options).

get_status(Pid) ->
    gen_server:call(Pid, get_status).

change_file(Pid) ->
    gen_server:call(Pid, change_file).

clean_old(Pid) ->
    gen_server:call(Pid, clean_old).

%%% Generic server.

-record(state, {
	  log, 			%% Log name. 				:: atom()
	  path, 		%% Current file path. 			:: filename()
	  iod, 			%% Open file. 				:: io_device()
	  acc_size, 		%% Lines written so far. 		:: line_count()
	  change_tmr, 		%% File change timer. 			:: timer_ref()
	  clean_tmr, 		%% Old file cleanup timer. 		:: timer_ref()
	  dir, 			%% Destination directory. 		:: filename()
	  size_limit, 		%% Change after N entries. 		:: line_count() | infinity
	  time_limit, 		%% Change after N hours. 		:: hours()
	  lifetime, 		%% Remove old logs after N days. 	:: days()
	  cache_size, 		%% Write buffer size. 			:: kilobytes()
	  cache_time, 		%% Write buffer timeout. 		:: msec()
	  with_node, 		%% Embed node name in file name. 	:: boolean()
	  suffix 		%% Log file suffix. 			:: string()
	 }).

init([Log]) ->
    %% The only reason for trap_exit is that we always want to close & rename logfile cleanly.
    process_flag(trap_exit, true),
    case configure(default(Log)) of
	{#state{dir = Dir} = State, _} when is_list(Dir) ->
	    #state{path = Path, dir = Dir} = State2 = open(State),
	    Regexp = regexp(State2),
	    proc_lib:spawn_link(fun () -> clean_open(Log, Dir, Regexp, Path) end),
	    {ok, State2#state{clean_tmr = schedule_clean_old()}};
	_ ->
	    exit({bad_configuration, Log})
    end.

handle_call({send_msg, Msg}, _, State) ->
    {reply, ok, write(State, Msg)};
handle_call(set_options, _, State) ->
    case configure(State) of
	{StateX, true} ->
	    State2 = change(StateX);
	{StateX, _} ->
	    State2 = StateX
    end,
    {reply, ok, State2};
handle_call(get_status, _, #state{path = Path, acc_size = AS} = State) ->
    {reply, {AS, Path}, State};
handle_call(change_file, _, State) ->
    {reply, ok, change(State)};
handle_call(clean_old, _, #state{clean_tmr = Timer} = State) ->
    erlang:cancel_timer(Timer),
    {reply, ok, cleanup(State)};
handle_call(_, _, State) ->
    {reply, {error, bad_request}, State}.

handle_info({timeout, CR, clean_old}, #state{clean_tmr = CR} = State) ->
    {noreply, cleanup(State)};
handle_info({timeout, CR, change}, #state{change_tmr = CR} = State) ->
    {noreply, change(State)};
handle_info({'EXIT', _, Reason}, State) when Reason /= normal ->
    {stop, Reason, State};
handle_info(_, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    %% XXX We've change #state{} layout, should have upgrade code here when tagging next release!
    %% XXX Also changed timer layout, handle that too.
    {ok, State}.

terminate(_, State) ->
    close(State).

%%% Implementation.

cleanup(#state{log = Log, lifetime = LT, dir = Dir} = State) ->
    Regexp = regexp(State),
    proc_lib:spawn_link(fun () -> clean_old(Log, Dir, Regexp, LT) end),
    State#state{clean_tmr = schedule_clean_old()}.

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
    open(close(State#state{change_tmr = nil})).

open(#state{time_limit = TL, iod = nil, cache_size = CS, cache_time = CT} = State) ->
    {ok, Iod} = file:open(Path = logfile(State), cache_opts([raw, write, binary], CS, CT)),
    State#state{iod = Iod, path = Path, change_tmr = schedule_file_change(TL)}.

cache_opts(Opts, CS, CT) when CS == 0; CT == 0 ->
    Opts;
cache_opts(Opts, CS, CT) ->
    [{delayed_write, CS * 1024, CT} | Opts].

close(#state{iod = nil} = State) ->
    State;
close(#state{iod = Iod, path = Path, acc_size = Size} = State) ->
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
    State#state{iod = nil, path = nil, acc_size = 0}.

clean_open(Log, Dir, Regexp, Cur) ->
    Strip = fun (F, N) when F /= Cur ->
		    file:rename(F, rootname(F, ".open")),
		    N + 1;
		(_, N) ->
		    N
	    end,
    case filelib:fold_files(Dir, Regexp ++ ".open", false, Strip, 0) of
	0 ->
	    ok;
	N ->
	    error_logger:info_msg("[Audit Log] Cleaned up ~d open files for ~s.", [N, Log])
    end.

clean_old(Log, Dir, Regexp, Limit) ->
    LT = erlang:localtime(),
    Clean = fun (F, N) ->
		    case calendar:time_difference(file_date(basename(F)), LT) of
			{D, _} when D >= Limit ->
			    ok = file:delete(F),
			    N + 1;
			_ ->
			    N
		    end
	    end,
    case filelib:fold_files(Dir, Regexp, false, Clean, 0) of
	0 ->
	    ok;
	N ->
	    error_logger:info_msg("[Audit Log] Cleaned up ~d old files for ~s.", [N, Log])
    end.

%%% Utilities.

default(Log) ->
    #state{log = Log, path = nil, iod = nil, change_tmr = nil, clean_tmr = nil, acc_size = 0, dir = nil}.

configure(#state{log = Log, cache_size = CS0, cache_time = CT0, size_limit = SL0, time_limit = TL0, dir = LD0,
		 lifetime = LT0, with_node = WN0, suffix = SU0} = State) ->
    [#audit_log_conf{opts = Opts}] = mnesia:dirty_read(audit_log_conf, Log),
    CS = get_value(cache_size, Opts, CS0),
    CT = get_value(cache_time, Opts, CT0),
    SL = get_value(size_limit, Opts, SL0),
    TL = get_value(time_limit, Opts, TL0),
    LT = get_value(lifetime, Opts, LT0),
    WN = get_value(with_node, Opts, WN0),
    SU = get_value(suffix, Opts, SU0),
    LD = get_value(dir, Opts, LD0),
    Change = (SL /= SL0) or (TL /= TL0) or (WN /= WN0) or (SU /= SU0),
    State2 = State#state{cache_size = CS, cache_time = CT, size_limit = SL, time_limit = TL,
			 lifetime = LT, with_node = WN, suffix = SU, dir = absname(LD)},
    {State2, Change}.

logfile(#state{dir = Dir, log = Log, with_node = With_node, suffix = Suf}) ->
    logfile(Dir, Log, With_node, Suf, nil).

logfile(Dir, Log, With_node, Suf, N) ->
    Timestamp = audit_log_lib:plain_ts(erlang:localtime()),
    if is_atom(N) ->
	    Seq = "";
       true ->
	    Seq = [$., make_two(N)]
    end,
    case With_node of
	true ->
	    File = [atom_to_list(Log), $_, node_to_list(), $_, Timestamp, Seq, file_suf(Suf)];
	_ ->
	    File = [atom_to_list(Log), $_, Timestamp, Seq, file_suf(Suf)]
    end,
    mkdir(dirname(Path = join([Dir, File]))),
    case filelib:is_file(Path) of
	true ->
	    logfile(Dir, Log, With_node, Suf, if is_atom(N) -> 1; true -> N + 1 end);
	_ ->
	    Path ++ ".open"
    end.

regexp(#state{log = Log, with_node = With_node, suffix = Suf}) ->
    flatten([atom_to_list(Log),
	     case With_node of
		 true ->
		     [$_, node_to_list(), $_];
		 _ ->
		     [$_]
	     end,
	     "[0-9]{14}([.][0-9][0-9])?", file_suf(Suf)]).

file_date(Basename) ->
    {match, [TS]} = re:run(Basename, ".*_(?<TS>[0-9]{14})([.]|$)", [{capture, ['TS'], binary}]),
    <<Y1, Y2, Y3, Y4, M1, M2, D1, D2, H1, H2, M3, M4, S1, S2>> = TS,
    {{dec(Y1, Y2, Y3, Y4), dec(M1, M2), dec(D1, D2)}, {dec(H1, H2), dec(M3, M4), dec(S1, S2)}}.

dec(N1, N2, N3, N4) ->
    (dig(N1) * 1000) + (dig(N2) * 100) + (dig(N3) * 10) + dig(N4).

dec(N1, N2) ->
    (dig(N1) * 10) + dig(N2).

dig(N) when is_integer(N), N >= $0, N =< $9 ->
    N - $0.

node_to_list() ->
    [App, Node] = string:tokens(atom_to_list(node()), "@"),
    [App, $_, Node].

file_suf("") ->
    [];
file_suf(Suf) ->
    [$., Suf].

mkdir(Path) ->
    mkdir(split(Path), []).

mkdir([Dir | Tail], Parent) ->
    case file:make_dir(Path = join([Parent, Dir])) of
	ok ->
	    ok;
	{error, eexist} ->
	    ok;
	{error, eisdir} ->
	    %% Reported on MacOS X 10.6.8 with file:make_dir("/").
	    ok;
	{error, Rsn} ->
	    exit({mkdir_failed, Rsn, Dir})
    end,
    mkdir(Tail, Path);
mkdir([], _) ->
    ok.

schedule_clean_old() ->
    erlang:start_timer(1000 * secs_to_midnight(), self(), clean_old).

schedule_file_change(Hours) ->
    erlang:start_timer(interval(erlang:now(), Hours), self(), change).

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
