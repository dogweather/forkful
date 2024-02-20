---
date: 2024-01-25 02:04:12.450554-07:00
description: "Logging is essentially jotting down what your application is up to\u2014\
  a diary, if you will, but for code. Programmers do it to keep track of the nitty-\u2026"
lastmod: 2024-02-19 22:05:18.940742
model: gpt-4-1106-preview
summary: "Logging is essentially jotting down what your application is up to\u2014\
  a diary, if you will, but for code. Programmers do it to keep track of the nitty-\u2026"
title: Logging
---

{{< edit_this_page >}}

## What & Why?
Logging is essentially jotting down what your application is up to—a diary, if you will, but for code. Programmers do it to keep track of the nitty-gritty, like state changes, system events, and pesky bugs, making sure no hiccups slip past unnoticed.

## How to:
In Fish, logging can be as simple as redirecting standard output and error streams to a file. Let's make a log entry for our script's start and end times. 

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script started" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script ended" >> my_app.log
end

log_start
# ... your script's tasks ...
log_end

cat my_app.log
```

Here's what you'd see in `my_app.log`:

```
2023-04-01 10:35:47  - Script started
2023-04-01 10:36:02  - Script ended
```

For advanced logging, you can utilize functions with parameters for log level and messages:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "This is an informational message."
log_message ERROR "Something went wrong!"
```

Sample `my_app.log` output will be:
```
2023-04-01 10:35:47 [INFO] This is an informational message.
2023-04-01 10:35:49 [ERROR] Something went wrong!
```

## Deep Dive
Historically, logging in shell scripts was done with a bunch of `echo` statements, and while this is certainly still an option, implementing more complex systems can be a challenge. Fish doesn't have a built-in logging mechanism like some other shells or programming languages do, so you often need to roll your own.

Alternatives to Fish's built-in `echo` command for logging include Unix tools like `syslog` or `logger`, which interface with the system log daemon, providing a more integrated approach to logging system-wide events.

Fish's simplicity allows you to create functions to handle the verbosity of logging, setting different levels that you can switch on or off. Some implementations can even include the name of the script, line number, and timestamp, which makes it easier to trace back through the steps that led to an event.

## See Also
- The Fish Shell Documentation on writing functions: https://fishshell.com/docs/current/#syntax-function
- Basic Shell Scripting Tips: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Guide to Syslog Protocol: https://tools.ietf.org/html/rfc5424
