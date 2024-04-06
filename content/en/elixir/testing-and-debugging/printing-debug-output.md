---
date: 2024-01-20 17:52:15.287957-07:00
description: "How to: This shows the simplest way to print something to the console\
  \ using `IO.inspect/2`. The label option adds a custom prefix, making the output\u2026"
lastmod: '2024-04-05T21:53:35.471144-06:00'
model: gpt-4-1106-preview
summary: This shows the simplest way to print something to the console using `IO.inspect/2`.
title: Printing debug output
weight: 33
---

## How to:
```elixir
defmodule DebugExample do
  def show_debug_output do
    name = "Elixir"

    IO.inspect(name, label: "Debug")
    # further processing
  end
end

DebugExample.show_debug_output()
# Output:
# Debug: "Elixir"
```

This shows the simplest way to print something to the console using `IO.inspect/2`. The label option adds a custom prefix, making the output easier to spot.

## Deep Dive
Elixir's `IO.inspect/2` function is akin to `puts` in Ruby or `console.log` in JavaScript. It's great for quick-and-dirty debugging, a practice as old as programming itself.

Alternatives in Elixir include using the `Logger` module for more systematic application-level logging. This is more configurable and suitable for production.

For implementation details, `IO.inspect/2` returns the given data, making it easy to insert into a pipeline without affecting functionality. Historically, Elixir has always emphasized developer tooling, and functions like `IO.inspect/2` embody this by making debugging a more integrated experience.

## See Also
- Elixir's IO module: https://hexdocs.pm/elixir/IO.html
- Introduction to debugging in Elixir: https://elixirschool.com/en/lessons/specifics/debugging
- Official guide to Logger: https://hexdocs.pm/logger/Logger.html
