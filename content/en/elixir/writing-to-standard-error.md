---
title:                "Elixir recipe: Writing to standard error"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Logging is an essential part of any programming language. It helps developers track and troubleshoot errors, as well as gather valuable information about the execution of their code. Writing to standard error is a common way to log errors in many languages, and Elixir is no exception.

## How To

To write to standard error in Elixir, we can use the `IO.puts/2` function and pass `"stderr"` as the first argument. Let's take a look at an example:

```
Elixir
IO.puts("This is an error!", "stderr")
```

The first argument is the message we want to output, and the second argument specifies that we want to write to standard error. Running this code will print out "This is an error!" to the standard error stream. We can also use `IO.inspect/2` to print out any data types to standard error, for example:

```
Elixir
IO.inspect([1, 2, 3], label: "List", to: "stderr")
```

This will output the following to standard error:

```
List: [1, 2, 3]
```

## Deep Dive

Behind the scenes, standard error is part of the I/O system in Elixir. The `IO.puts/2` and `IO.inspect/2` functions are convenience functions that handle writing to standard error for us. However, we can also use the `IO.device/2` function to specify that we want to write to `:stdio.con` which represents standard error. This gives us more control and flexibility over our logging.

Additionally, we can use `:stderr.bin` as an argument for the `IO.binstream/2` function to create a binary stream that can be used for writing to standard error. This is useful for advanced logging scenarios that require writing data in a specific format.

## See Also

- [Elixir documentation on I/O](https://hexdocs.pm/elixir/IO.html)
- [Elixir Flow documentation](https://hexdocs.pm/elixir/Flow.html)
- [Elixir Logger documentation](https://hexdocs.pm/logger/Logger.html)