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

Writing to standard error is a way for programmers to output information about errors and warnings that occur during the execution of their code. This is an important tool for debugging and troubleshooting programs as it allows developers to see what went wrong and how to fix it.

## How to:

To write to standard error in Gleam, use the stdlib `io/2` module and the `write_error` function. This function takes in a string as an argument and outputs it to the standard error stream. Here's an example:

```
fn main() {
  let message = "Oops, something went wrong";
  io::write_error(message);
}
```

The output will be:
```
Oops, something went wrong
```

You can also specify the standard error stream's file descriptor with the `write_error_to_fd` function. This can be useful for redirecting error messages to a specific location. Here's an example:

```
fn main() {
  let message = "Oops, something went wrong";
  let fd = 2; // file descriptor for standard error stream
  io::write_error_to_fd(message, fd);
}
```

The output will be the same as before, but now it has been directed to the specified file descriptor.

## Deep Dive:

Historically, writing to standard error has been a common practice in programming languages. It allows developers to differentiate between normal output and error messages, making debugging and troubleshooting easier.

An alternative to writing to standard error is using the `panic` function, which stops the execution of the program and outputs an error message. However, this can be seen as a harsh approach and may not always be desirable.

Implementation-wise, writing to standard error in Gleam uses the operating system's standard error stream. This means that the output will be visible in the console or terminal where the program is being executed.

## See Also:

- [Gleam stdlib documentation for `io/2` module](https://gleam.run/documentation/stdlib/io/)
- [Difference between standard output and standard error](https://www.theserverside.com/feature/Standard-Output-vs-Standard-Error-What-DevOps-needs-to-know)