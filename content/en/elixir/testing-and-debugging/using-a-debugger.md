---
date: 2024-01-25 20:50:36.208411-07:00
description: "Using a debugger in Elixir involves stepping through your code, inspecting\
  \ variables, and tracking flows to squash bugs. Programmers do it to make sense\u2026"
lastmod: '2024-03-13T22:44:59.786465-06:00'
model: gpt-4-1106-preview
summary: "Using a debugger in Elixir involves stepping through your code, inspecting\
  \ variables, and tracking flows to squash bugs. Programmers do it to make sense\u2026"
title: Using a debugger
---

## How to:
Elixir ships with a built-in graphical debugger called `:debugger`. To use it, you'll need to start it and attach to your running process.

First, ensure you have `:debugger` started within an `iex` session:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Now, interpret the code module you want to debug:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

You can set a breakpoint:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

And then, run your function to hit the breakpoint and step through your code:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# Debugger will pause execution at the line with the breakpoint
```

## Deep Dive
Before Elixir's `:debugger`, Erlang provided the debugger that Elixir uses; it's robust and great at handling concurrent processes, a sweet spot of Erlang VM (BEAM). Unlike some other debuggers, `:debugger` doesn't allow modification of variables on the fly, due to the immutable nature of data in Elixir. As for alternatives, you have `IEx.pry` that lets you pause execution and jump into a REPL at any point in your code, which can be super handy. 

While `:debugger` is good for a graphical interface, some might prefer the built-in `:observer` tool that also offers process inspection and system metrics, albeit not specifically targeted at stepping through code. Elixir's community also contributes tools like `visualixir` and `rexbug`, expanding the ecosystem of debug tools beyond the defaults.

## See Also
- Official Elixir Getting Started Guide on Debugging: https://elixir-lang.org/getting-started/debugging.html
- Erlang's `:debugger` Documentation: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Elixir Forum Discussions on Debugging Techniques: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
