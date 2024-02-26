---
date: 2024-01-21 21:19:08.204031-07:00
description: "Handling errors means writing code that can deal with things going sideways.\
  \ Programmers do it to prevent crashes and to make sure their programs can\u2026"
lastmod: '2024-02-25T18:49:56.252433-07:00'
model: gpt-4-1106-preview
summary: "Handling errors means writing code that can deal with things going sideways.\
  \ Programmers do it to prevent crashes and to make sure their programs can\u2026"
title: Handling errors
---

{{< edit_this_page >}}

## What & Why?

Handling errors means writing code that can deal with things going sideways. Programmers do it to prevent crashes and to make sure their programs can recover gracefully when Murphy’s Law strikes.

## How to:

In Elixir, we often use pattern matching and the `case` statement to handle different outcomes, including errors.

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "Cannot divide by zero."}
      _ -> {:ok, a / b}
    end
  end
end

# Successful division
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2 is #{result}")

# Attempt to divide by zero
{:error, reason} = Example.divide(10, 0)
IO.puts("Error: #{reason}")
```

Sample output:
```
10 / 2 is 5.0
Error: Cannot divide by zero.
```

When you run this Elixir code, you'll either get a successful division or an error message, depending on your input. No crashes here!

## Deep Dive

Way back, error handling was often about checking return values. With Elixir’s functional roots though, we've got pattern matching and tagged tuples, like `{:ok, value}` or `{:error, reason}`, which are more elegant.

There are other ways to handle errors in Elixir:

- **Elixir's `try` and `rescue`** which resemble the traditional `try-catch` in imperative languages but are used less frequently due to Elixir's preference for explicitness.
- **Supervisors and GenServers**, part of Elixir's OTP framework, which are more about fault tolerance. They watch your code's process, ready to restart it if things go awry.

Implementation-wise, Elixir builds on Erlang's robustness. It treats errors as just another type of message to be handled with all the pattern matching and functional goodness.

## See Also

For further reading on error handling in Elixir, check out:

- Elixir's official guide on [error handling](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- Learn more about [processes and OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- The Elixir Forum is always a good place to ask questions: [https://elixirforum.com](https://elixirforum.com).
