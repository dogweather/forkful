---
changelog:
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:02:40.198503-07:00
description: "How to: Elixir's standard library, through the `DateTime` module, allows\
  \ fetching the current date and time. Since Elixir runs on the Erlang VM (BEAM),\
  \ it\u2026"
lastmod: '2024-03-13T22:44:59.791562-06:00'
model: gpt-4-0125-preview
summary: Elixir's standard library, through the `DateTime` module, allows fetching
  the current date and time.
title: Getting the current date
weight: 29
---

## How to:
Elixir's standard library, through the `DateTime` module, allows fetching the current date and time. Since Elixir runs on the Erlang VM (BEAM), it leverages the underlying Erlang functionalities for time operations.

### Using Elixir's Standard Library
Elixir provides the `DateTime.utc_now/0` function to get the current date and time in UTC.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**Sample Output:**
```
~U[2024-02-05 19:58:40.925931Z]
```

To get just the current date, you might extract the year, month, and day components:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**Sample Output:**
```
~D[2023-05-04]
```

### Using the Timex Library
For more complex date-time requirements, a popular third-party library called Timex can be utilized. First, add `Timex` to your mix.exs dependencies:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

After installing the dependency (`mix deps.get`), you can use Timex to get the current date:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**Sample Output:**
```
~D[2023-05-04]
```

Timex offers extensive functionalities for date-time manipulation, making it a powerful addition to your Elixir applications especially when dealing with time zones, formatting, and parsing of dates and times.

By understanding and utilizing Elixir's built-in capabilities and the Timex library, you can easily work with dates and times in your Elixir applications, tailoring the experience to the needs of your application with precision and ease.
