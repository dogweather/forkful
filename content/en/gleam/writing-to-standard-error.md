---
title:                "Writing to standard error"
html_title:           "Gleam recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error is the practice of outputting error information from a program. Programmers use it to help debug or track issues without interrupting the normal output of the program.

## How to:

In Gleam, you can write to standard error using `stdout` and `stderr` from the `gleam/io` module. Here's a simple example:

```Gleam
import gleam/io.{stdout, stderr}

pub fn main(args: List(String)) -> Nil {
  case args {
    [] ->
      stderr("No arguments provided!")
      Ok(Nil)

    _ ->
      stdout("Program executed successfully.")
      Ok(Nil)
  }
}
```

In this example, if no arguments are passed to the function, it will write "No arguments provided!" to the standard error. If there are any arguments, it will write "Program executed successfully." to the standard output.

## Deep Dive

Historically, the concept of separating error messages from regular output comes from Unix-like operating systems where there are different file descriptors for each.

As an alternative, some choose to write errors directly to the standard output. This merges all output into a single stream, but can make it harder to distinguish between the normal and error outputs.

In Gleam, writing to standard error is done using the Erlang's `:io.format(:standard_error, format_string, args)` function under the hood, providing consistent behaviour with the Erlang/OTP ecosystem.

## See Also

- Gleam IO Module Documentation: https://hexdocs.pm/gleam_stdlib/gleam/io/
- Quick introduction to file descriptors: https://www.gnu.org/software/libc/manual/html_node/File-Descriptors.html
- Erlang 'io' Module Documentation: http://erlang.org/doc/man/io.html