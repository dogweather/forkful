---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is the process of displaying intermediate outputs or values to troubleshoot a piece of code. Programmers do this to find the source of errors, validate their logic, and understand the execution flow. 

## How to:

Here's a simple way of printing debug output using Elixir's IO module:

```Elixir
IO.puts("Hello, World!")
```

This code will display "Hello, World!" on your terminal. 

If you want to debug a specific value during code execution, use IO.inspect:

```Elixir
value_to_debug = "Testing"

IO.inspect(value_to_debug)
```

As you run the code, it will print "Testing", giving you the value of the dedicated variable. 

## Deep Dive

### Historical Context
The practice of printing debug output dates back to the very early stages of programming. Its simplicity and immediate feedback make it a handy tool even in highly advanced languages like Elixir. 

### Alternatives
While printing debug output is straightforward, it may not serve in complex scenarios. For such cases, Elixir provides the use of debugging tools like `IEx.pry` for interactive debugging, and `:debugger` module for visual debugging. 

```Elixir
defmodule Test do
  def run(value) do
    require IEx; IEx.pry #prying into the function
    value * 2
  end
end
```

### Implementation Details
Under the hood, when you use `IO.puts`, it communicates with the Erlang runtime system's IO server that handles all IO tasks. Remember, Elixir is built on top of Erlang, so you might come across this relationship now and then.

For `IO.inspect`, it is a bit smarter. It returns the value it's inspecting, so it doesn't interfere with your pipeline operations. It converts the data into a readable format, communicates with IO server, and finally prints the debug information on your terminal.

## See Also

Read more on Elixir's documentation: 
- IO module: [https://hexdocs.pm/elixir/IO.html](https://hexdocs.pm/elixir/IO.html)
- Debugging in Elixir: [https://hexdocs.pm/iex/IEx.Helpers.html#pry/0](https://hexdocs.pm/iex/IEx.Helpers.html#pry/0)
- Erlang Runtime System: [https://erlang.org/doc/man/erl.html](https://erlang.org/doc/man/erl.html)