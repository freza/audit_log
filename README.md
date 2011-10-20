# audit_log

## Purpose

Lightweight audit logging library.

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
* Logfile name embeds creation timestamp.

## Public interface

### Type definitions

```erlang
Args         = [term()].
Err          = {error, term()}.
Fmt          = string().
KB = MS      = integer() >= 0.
Log          = atom().
NN = HH = DD = integer() > 0 | infinity.
Stat         = {Status, Written, Cur_file}.
Opt          = {cache_size, KB} | {cache_time, MS} | {size_limit, NN} | {time_limit, HH} | {lifetime, DD} | {dir, Path}.
```

### User API

```erlang
audit_log:syslog_msg(Fmt, Args)     -> ok.
audit_log:syslog_msg(iodata())      -> ok.

audit_log:audit_msg(Log, Fmt, Args) -> ok.
audit_log:audit_msg(Log, iodata())  -> ok.
```

### Management API

```erlang
audit_log:open_log(Log)             -> ok | Err.
audit_log:open_log(Log, [Opt])      -> ok | Err.
audit_log:close_log(Log)            -> ok.
audit_log:change_file(Log)          -> ok | Err.
audit_log:clean_old(Log)            -> ok | Err.
```

### Maintenance API

```erlang
audit_log:get_config()              -> [{Log, Opt}].
audit_log:get_config(Log)           -> [Opt].

audit_log:set_config([Opt])         -> ok.
audit_log:set_config(Log, [Opt])    -> ok.

audit_log:get_status()              -> [{Log, Stat}].
audit_log:get_status(Log)           -> Stat.
```

## Application environment

```
application:set_env(audit_log, default_dir, Dir).

    Default target directory for new logs, defaults to "$ROOT/audit_logs/".
```
