---
title:                "Writing to standard error"
html_title:           "Elixir recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error is a way for programmers to print out error messages or information in the Elixir programming language. This is useful for debugging and troubleshooting code, as it allows developers to see any errors or issues that may arise while the program is running.

## How to:
To write to standard error in Elixir, you can use the `IO.puts/2` function with the `:stderr` option as the first argument. Here is an example:

```Elixir
IO.puts(:stderr, "This is an error message.")
```

This will print "This is an error message." to the standard error stream. Here is the output:

```Elixir
This is an error message.
```

You can also use the `IO.write/2` function to write to standard error without adding a new line at the end. Here is an example:

```Elixir
IO.write(:stderr, "This is an error message.")
```

This will print "This is an error message." without a new line at the end. Here is the output:

```Elixir
This is an error message.
```

## Deep Dive:
Writing to standard error has been a common practice in programming languages for decades. It originated from the C programming language, where the `fprintf` function was used to write to the standard error stream. Standard error is considered a special stream in programming languages, separate from the standard output stream, as it is used specifically for displaying error messages.

An alternative to writing to standard error is using the `raise/2` function, which can be used to raise an exception and output a custom error message. However, this does not print the error message to the standard error stream by default, so developers may choose to use `IO.puts/2` or `IO.write/2` instead.

When writing to standard error in Elixir, the output may differ depending on the terminal you are using. Some terminals may display the output in a different color or format to differentiate it from standard output. It may also be useful for developers to use the `inspect/2` function to format the error message in a specific way before printing it to standard error.

## See Also:
- [Elixir documentation on IO](https://hexdocs.pm/elixir/IO.html)
- [Elixir forum discussion on writing to standard error](https://elixirforum.com/t/writing-to-standard-error-io-stdout-stderr/3938/2)