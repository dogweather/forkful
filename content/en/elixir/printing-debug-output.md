---
title:                "Printing debug output"
html_title:           "Elixir recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is a means of examining your Elixir program as it runs, displaying values and states of elements to the console for inspection. It’s crucial during development or debugging as it helps to understand the actual sequence of code execution or to check some values/classes/structures during the runtime.

## How To:

Elixir provides several methods for debug output, the most common ones are `IO.puts/2` and `IO.inspect/2`. Here is an example:

```Elixir
x = 5
IO.puts("The value of x is #{x}")
```
```Output
The value of x is 5
```
`IO.puts/2` prints the argument given to the standard output.

```Elixir
list = [1, 2, 3, 4, 5]
IO.inspect(list)
```
```Output
[1, 2, 3, 4, 5]
```
`IO.inspect/2` can be even more valuable as it can handle more complex data types, printing a detailed, readable output of your data's structure.

## Deep Dive

Originally with roots in Erlang, Elixir brings along the `io` module from its parent language. While it may not seem as fancy as integrated development environment (IDE) debuggers, in-place printing has value in its simplicity and directness. 

There are alternatives to directly printing to the console, for instance, using Elixir's powerful tracing and debugging tools like `:debugger` module or libraries like `:observer`. These tools allow for stepping through code and inspecting program state at different points of execution, offering a deeper but more complex approach.

Using `IO.inspect/2` directly modifies the codebase which might not be the best practice. One preferred approach is to use a logging library, like `Logger`, that won't interfere with production code and offers various options for output format and destination.

## See Also

- [IO.puts/2 official documentation](https://hexdocs.pm/elixir/IO.html#puts/2)
- [IO.inspect/2 official documentation](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [`:debugger` official documentation](https://erlang.org/doc/man/debugger.html)
- [`:observer` official documentation](https://erlang.org/doc/man/observer.html)