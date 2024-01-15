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

## Why

Writing to the standard error (often abbreviated as "stderr") is a crucial function in programming that allows developers to efficiently handle error messages and debugging in their code. Without utilizing stderr, it can be difficult to pinpoint and troubleshoot issues within a program.

## How To

To write to stderr in Gleam, you can use the `io.println` function and specify stderr as the first argument. Here's an example:

```Gleam
import gleam/io

io.println(gleam/core.Line(stderr), "This is an error message!")
```

This will print the given message to the standard error stream. You can also use `gleam/core.Line(stderr)` to define a new line before the message to make it stand out.

### Sample Output

Running this code will output the following to your terminal: 

```
This is an error message!
```

## Deep Dive

Writing to standard error can be particularly useful when dealing with error handling or debugging in your program. By directing certain messages to stderr instead of stdout (the standard output stream), you can easily distinguish and filter out error messages from regular output. This can help with troubleshooting and identifying issues within your code.

It's also worth noting that stderr and stdout can be redirected to different locations, such as a log file, to store and review error messages separately from regular output.

## See Also 

- [Gleam documentation for `io.println`](https://gleam.run/core/io.html#println)
- [Introduction to stderr and stdout in programming](https://www.geeksforgeeks.org/stderr-stdout-linux/)
- [Using stderr for error handling in C programming](https://www.thecrazyprogrammer.com/2013/06/using-stderr-for-error-messages-in-c.html)