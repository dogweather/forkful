---
title:                "Запис в стандартний потік помилок"
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Writing to standard error (`stderr`) is for error messages and diagnostics. We use it to separate regular program output from error information. It helps in debugging and log analysis.

## How to: (Як це зробити:)
Gleam's `io` module lets us write to `stderr`. Here's a simple usage example and what you'll see on your screen:
```gleam
import gleam/io

pub fn main() {
  io.println("This is standard output")
  io.eprintln("This is standard error")
}
```
Output:
```
This is standard output
```
Error:
```
This is standard error
```

## Deep Dive (Поглиблений Розгляд)
Historically, separating output and errors helps in UNIX, where you can redirect them individually. Alternatives include writing to a file or network service for logging. Implementation-wise, Gleam relies on Erlang's capabilities for `stderr`, as it compiles to Erlang bytecode.

## See Also (Дивіться також)
- Gleam's `io` module documentation: https://hexdocs.pm/gleam_stdlib/gleam/io/
- Erlang's error logging: http://erlang.org/doc/apps/error_logger/
- Unix Philosophy on Standard Streams: https://en.wikipedia.org/wiki/Unix_philosophy#Pipelines
