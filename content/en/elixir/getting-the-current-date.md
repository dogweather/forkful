---
title:                "Elixir recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to get the current date in your Elixir program? Whether it's for displaying information or for performing some sort of date-based logic, knowing how to retrieve the current date can be a useful skill for any Elixir programmer. In this blog post, we will explore how to get the current date in Elixir and dive into the underlying mechanisms behind it.

## How To
To get the current date in Elixir, we can use the `DateTime.now()` function. Let's see an example of how this works in a code block:

```elixir
iex> DateTime.now()
{:ok, ~N[2021-05-26 15:45:10.185597]}
```

This function returns a tuple with the first element being `:ok` and the second element being a `DateTime` struct. This struct contains information about the current date and time, including the year, month, day, hour, minute, second, and millisecond. We can also specify a specific timezone by passing in a string as the second argument to `DateTime.now()`.

```elixir
iex> DateTime.now("America/New_York")
{:ok, #DateTime<2021-05-26 11:45:10.185597-04:00>}
```

We can also use the `Caleandar.ISO` module to format the output of the `DateTime` struct. Let's see an example:

```elixir
iex> DateTime.now() |> Calendar.ISO.format("{ISO}")
"2021-05-26T15:45:10.185597Z"
```

The `{ISO}` format option will return the date and time in ISO 8601 format, which is a common standard for representing date and time information.

## Deep Dive
Under the hood, Elixir uses the Erlang `calendar` module to get the current date and time. This module is part of Erlang's standard library and provides functions for working with calendars and dates. When we call `DateTime.now()`, Elixir calls the `calendar.universal_time_to_local_time/1` function from the `calendar` module and passes in the current time in milliseconds. This function then converts the milliseconds into a tuple containing the year, month, day, hour, minute, second, and millisecond.

It's also worth noting that `DateTime.now()` is dependent on the system clock of the machine running the Elixir program. If the system clock is inaccurate or changes while the program is running, it may affect the result returned by `DateTime.now()`. To avoid this, we can use `DateTime.now!("utc")` to retrieve the current date and time in UTC regardless of the system clock's accuracy.

## See Also
- [Elixir DateTime module docs](https://hexdocs.pm/elixir/DateTime.html)
- [Erlang calendar module docs](http://erlang.org/doc/man/calendar.html)
- [ISO 8601 Wikipedia page](https://en.wikipedia.org/wiki/ISO_8601)