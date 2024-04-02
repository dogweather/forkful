---
date: 2024-02-03 19:03:54.587811-07:00
description: "Writing to standard error (stderr) is about directing error messages\
  \ and diagnostics to the stderr stream, separate from standard output (stdout).\u2026"
lastmod: '2024-03-13T22:44:59.760246-06:00'
model: gpt-4-0125-preview
summary: "Writing to standard error (stderr) is about directing error messages and\
  \ diagnostics to the stderr stream, separate from standard output (stdout).\u2026"
title: Writing to standard error
weight: 25
---

## What & Why?
Writing to standard error (stderr) is about directing error messages and diagnostics to the stderr stream, separate from standard output (stdout). Programmers do this to differentiate regular program output from error messages, allowing for more effective debugging and logging.

## How to:
In Clojure, you can write to stderr using the `*err*` stream. Here’s a basic example:

```clojure
(.write *err* "This is an error message.\n")
```

Note that after writing a message, you should flush the stream to ensure the message is immediately output:

```clojure
(flush)
```

Sample output to stderr:
```
This is an error message.
```

If you're handling exceptions, you might want to print stack traces to stderr. Use `printStackTrace` for this:

```clojure
(try
  ;; Code that might throw an exception
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

For more structured error logging, third-party libraries like `timbre` can be configured to log to stderr. Here's a basic setup and usage:

First, add `timbre` to your dependencies. Then configure it to use stderr:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; Disable stdout logging
(timbre/set-config! [:appenders :spit :enabled?] false) ;; Disable file logging
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; Enable stderr for errors

(timbre/error "An error occurred while processing your request.")
```

This will direct error-level messages to stderr, making them distinct from standard application output.
