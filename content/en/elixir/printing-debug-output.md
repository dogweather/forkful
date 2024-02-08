---
title:                "Printing debug output"
aliases:
- en/elixir/printing-debug-output.md
date:                  2024-01-20T17:52:15.287957-07:00
model:                 gpt-4-1106-preview
simple_title:         "Printing debug output"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output in Elixir involves displaying interim results or variable values in the console. Programmers do this to track down bugs or to understand what their code is doing at a particular point in execution.

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
