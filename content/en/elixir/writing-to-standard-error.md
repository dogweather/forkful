---
title:    "Elixir recipe: Writing to standard error"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error, also known as "stderr", is an important aspect of Elixir programming. It allows developers to output error messages, warnings, and other information to the terminal, making it easier to troubleshoot and debug code. In this blog post, we will explore why writing to standard error is crucial in Elixir programming.

## How To

In Elixir, writing to standard error is achieved through the `IO.puts/2` function. This function takes two arguments: the message to be outputted and the standard error stream. Let's take a look at an example:

```Elixir
IO.puts("An error has occurred.", :stderr)
```

When this code is executed, the message "An error has occurred" will be outputted to the standard error stream, denoted by the `:stderr` argument.

Another useful function for writing to standard error is `IO.inspect/2`. This function takes a value and outputs it to the standard error stream, making it useful for debugging purposes. Let's see an example:

```Elixir
list = [1, 2, 3]
IO.inspect(list, :stderr)
```

The output of this code will be the value of `list` printed to the standard error stream, which can help developers track the value of variables during program execution.

## Deep Dive

Writing to standard error in Elixir is not just limited to simple messages and values. It is also possible to colorize output to make it easier to distinguish between different types of messages. Elixir provides the `IO.ANSI` module for this purpose. Let's take a look at an example:

```Elixir
IO.puts(IO.ANSI.red("This is an error message.", :stderr))
```

In this code, the message "This is an error message" will be printed to the standard error stream in red text. This can be useful for highlighting important error messages in a sea of other output.

## See Also

- [Elixir Documentation on IO module](https://hexdocs.pm/elixir/IO.html)
- [Elixir Documentation on IO.ANSI module](https://hexdocs.pm/elixir/IO.ANSI.html)