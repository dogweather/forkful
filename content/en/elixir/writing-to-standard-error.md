---
title:    "Elixir recipe: Writing to standard error"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

As programmers, we often encounter errors in our code that need to be debugged. One way to track these errors is by writing them to the standard error, a stream that is separate from the standard output. By doing this, we can easily differentiate between regular program output and error messages, making it easier to quickly identify and address issues in our code.

## How To

Writing to standard error in Elixir is quite simple. There are a few different methods we can use, depending on what we want to achieve.

#### Using `IO.puts/2`

The most basic way to write to standard error is by using the `IO.puts/2` function. Its first argument is the stream we want to write to, and the second argument is the message we want to write. In this case, we would use `:stderr` as the first argument.

``` Elixir
IO.puts(:stderr, "This is an error message.")
```

The above code would output "This is an error message." to the standard error stream.

#### Using `File.write/3`

We can also write to standard error by using the `File.write/3` function. Similar to `IO.puts/2`, the first argument is the stream we want to write to, but this time we also need to specify the file we want to write to. We can use the special file `:stderr` to refer to the standard error stream.

``` Elixir
File.write(:stderr, "error.txt", "This is an error message.")
```

This code would write "This is an error message." to the `error.txt` file on the standard error stream.

#### Using `Logger`

Elixir also provides a built-in `Logger` module for logging purposes, which has the ability to write to standard error. We can use the `Logger.error/1` function to write a message to standard error.

``` Elixir
Logger.error("This is an error message.")
```

This code would output "This is an error message." to the standard error stream.

## Deep Dive

In Elixir, similar to the standard output, the standard error stream is line-buffered. This means that whenever we write to standard error, the output will only be displayed once we reach the end of the line or when we explicitly flush the stream.

To flush the standard error stream, we can use the `IO.flush/2` function, passing in `:stderr` as the first argument.

``` Elixir
IO.flush(:stderr)
```

Additionally, we can also use the `Logger` module to specify a level for our error message. By default, the `Logger.error/1` function writes to the standard error stream, but we can also use `Logger.log/2` and specify `:error` as the second argument.

``` Elixir
Logger.log(:error, "This is an error message.")
```

## See Also

- [Elixir documentation on standard I/O](https://hexdocs.pm/elixir/IO.html)
- [Elixir documentation on File module](https://hexdocs.pm/elixir/File.html)
- [Elixir documentation on Logger module](https://hexdocs.pm/elixir/Logger.html)