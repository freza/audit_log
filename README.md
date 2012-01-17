# audit_log

## Purpose

Lightweight audit logging library.

## Status

```
[ ] Prototype
[ ] Development
[X] Production
```

## History

* 2005-2010 Chandrashekhar Mullaparthi
* 2010-2011 Jachym Holecek

## Features

* Persistent logs (automatically opened on startup).
* Log messages treated as opaque data.
* Synchronous interface.
* Good throughput.
* Automatic file change when size or age limit reached.
* Automatic cleanup of old logfiles.
* All options configurable per log.

## Public interface

### Type definitions

```erlang
Args         = [term()].
Fmt          = string().
KB = MS      = integer() >= 0.
Log          = atom().
NN = HH = DD = integer() > 0 | infinity.
Stat         = {Items_written, Cur_file}.
Opts         = [Opt]
Opt          = {cache_size, KB}
             | {cache_time, MS}
             | {size_limit, NN}
             | {time_limit, HH}
             | {lifetime, DD}
             | {with_node, Flag}
             | {suffix, Str}
             | {dir, Path}.
```

### Log Options

```erlang
{cache_size, KB}
```

* Maximum file driver write buffer size, in kilobytes. Flush is forced when this
  threshold is reached. The value of `0` disables write buffering. Default value
  is 128KB.

```erlang
{cache_time, MS}
```

* File driver write buffer time to live, in milliseconds. Flush is forced when
  oldest buffered entry exceeds this age. The value of `0` disables write buffering.
  Default value is 1000ms.

```erlang
{size_limit, NN}
```

* Maximum number of entries per log file before file change is forced. Default value
  is 200k.

```erlang
{time_limit, HH}
```

* Maximum age, in hours, of log file before file change is forced. Change time will
  be aligned to multiples of this setting. Default value is 24h, aligns to midnight.

```erlang
{lifetime, DD}
```

* Maximum age, in days, of log file before it's automatically cleaned up. Old files
  are automatically deleted every midnight, but can be forced manually.

```erlang
{with_node, Flag}
```

* Boolean indicating whether to embed node name into file names for this log. Defaults
  to `false`.

```erlang
{suffix, Str}
```

* Suffix to use for log file names, excluding the leading ".". Defaults to "audit".

```erlang
{dir, Path}.
```

* Directory in which log files should be located. See also `default_dir` application
  option.

### User API

```erlang
audit_log:syslog_msg(Fmt, Args) -> ok.
audit_log:syslog_msg(iodata()) -> ok.

audit_log:audit_msg(Log, Fmt, Args) -> ok.
audit_log:audit_msg(Log, iodata()) -> ok.
```

* Write message to given log, or implicitely created `syslog`. After a call to this
  function returns, it is guaranteed that message has hit disk or file driver write
  cache, if enabled.

### Management API

```erlang
audit_log:open_log(Log[, Opts]) -> {ok, started | running | added} | {error, _}.
audit_log:close_log(Log) -> ok.
```

* Open or close given log. Opening involves recording log (and its default options,
  if any) into Mnesia and starting corresponding disk writer process. Symetrically,
  closing stops disk writer process and removes log from persistent configuration.
  These functions may be called even if `audit_log` application isn't running yet,
  such as during early system installation.

```erlang
audit_log:change_file(Log) -> ok | {error, _}.
```

* Close current logfile and open a new one. Provided for operator convenience as
  this is normally done automatically according to log options.

```erlang
audit_log:clean_old(Log) -> ok | {error, _}.
```

* Cleanup of logfiles. Provided for operator convenience as this is normally done
  automatically according to log options.

```erlang
audit_log:rediscover_logs() -> [{Log, Ret}].
```

* Ensure all currently known logs are up and running. `Ret` is the return value
  of `audit_log:open_log/N`.

### Maintenance API

```erlang
audit_log:get_config() -> [{Log, Opt}].
audit_log:get_config(Log) -> Opts.
```

* Return complete current configuration of all logs or of single selected log.

```erlang
audit_log:set_config([Log, ]Opts) -> ok.
```

* Apply selected configuration values to all logs, or to single selected log. Options
  are merged with existing configuration.

```erlang
audit_log:get_status() -> [{Log, Stat | down}].
audit_log:get_status(Log) -> Stat.
```

* Return runtime status of all logs, or single selected log.

### Utility API

```erlang
audit_log_lib:printable_date() -> iolist().
audit_log_lib:printable_date(now() | datetime()) -> iolist().
```

* Format current or given point in time as "YYYY-MM-DD_hh:mm:ss". If a value of now
  is given the format will have millisecond precision: "YYYY-MM-DD_hh:mm:ss.nnn".
  Note that return value isn't a flat string but an `iolist()`.

## Application environment

```erlang
application:set_env(audit_log, default_dir, Dir).
```

* Default target directory for log files, defaults to "$ROOT/audit_logs/".

## HOWTO

* Obtain and compile:

```sh
$ git clone git://github.com/eeltd/audit_log.git
$ cd audit_log.git
$ ./rebar clean && ./rebar compile && ./rebar xref
```

* Try out afterwards:

```sh
$ cd /tmp
$ erl -pa /where/ever/audit_log.git/ebin -boot start_sasl -sname foo -audit_log default_dir '"/tmp/audit_logs"'
[...]
(foo@bar)1> mnesia:create_schema([node()]).
[...]
(foo@bar)2> application:start(mnesia).
[...]
(foo@bar)3> application:start(audit_log).
[... In other terminal run: tail -f /tmp/audit_logs/syslog*.open ...]
(foo@bar)4> audit_log:syslog_msg([audit_log_lib:printable_date(), " Hello!", $\n]).
```
