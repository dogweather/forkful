---
title:                "Getting the current date"
date:                  2024-01-20T15:13:36.063434-07:00
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Fetching the current date in a program is like asking, "Hey, what's today's date?" We do this to timestamp events, handle scheduling, or just show users what day it is.

## How to:
Elixir makes getting the current date straightforward using the `Date` module:

```elixir
# Fetch the current date
current_date = Date.utc_today()

# Print it out
IO.inspect(current_date)
```

Sample output:

```elixir
~D[2023-04-06]
```

## Deep Dive
Back in the day, programmers dealt with more primitive languages and had to manually calculate dates based on seconds since an epoch (usually January 1, 1970). Nowadays, Elixir provides the `Date` module, simplifying date handling.

Alternatives include using `DateTime.utc_now()` if you need the exact time besides the date, or `NaiveDateTime.local_now()` if you're working with local time without timezone information.

Underneath the hood, Elixir relies on Erlang's time handling capabilities. When you call `Date.utc_today()`, it interfaces with Erlang to get Coordinated Universal Time (UTC).

## See Also
- Elixir's `Date` module documentation: https://hexdocs.pm/elixir/Date.html
- Elixir's `DateTime` module for more complex time-related tasks: https://hexdocs.pm/elixir/DateTime.html
- Introduction to `NaiveDateTime`: https://hexdocs.pm/elixir/NaiveDateTime.html
