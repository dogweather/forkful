---
title:                "Calculating a date in the future or past"
html_title:           "Elixir recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calculating Future and Past Dates in Elixir: An Insightful Guide

## What & Why?

Calculating a future or past date is about determining a specific date relative to a given one. Programmers do this for tasks like setting expirations for cookies, scheduling events, or tracking deadlines—any situation where date manipulation is involved.

## How to:

In Elixir, the `DateTime` module provides functions to manipulate dates. Here's how you add or subtract days:

```Elixir
dt = DateTime.utc_now()
# DateTime<2022-03-28 11:04:04Z>

DateTime.add(dt, 7*24*60*60, :second)
# DateTime<2022-04-04 11:04:04Z>

DateTime.add(dt, -7*24*60*60, :second)
# DateTime<2022-03-21 11:04:04Z>
```

This code uses `DateTime.utc_now()` to get the current date and time. To add/subtract days, `DateTime.add` adds/subtracts seconds from the date (7 days * 24 hours/day * 60 minutes/hour * 60 seconds/minute).

## Deep Dive

While Elixir is a fairly new programming language, conceived in 2012, its `DateTime` library has a good deal of versatility borrowed from Erlang. Before Elixir's approach, calculating future/past dates in Erlang posed some difficulties due to the language's immutability.

However, if Elixir's `DateTime` isn’t to your liking, you have alternatives!

One of these is the `Timex` library, a rich, versatile date/time library for Elixir that offers more functionalities:

```Elixir
use Timex

Timex.now() |> Timex.shift(days: 2)
# "2022-03-30T11:04:04Z"

Timex.now() |> Timex.shift(days: -2)
# "2022-03-26T11:04:04Z"
```

Also, Elixir computations are made using simple arithmetic rather than objects and methods that some other languages use. This means there's less room for unexpected behavior.

## See Also

1. [Elixir's Official DateTime docs](https://hexdocs.pm/elixir/DateTime.html)
2. [Elixir School guide to Dates and Times](https://elixirschool.com/en/lessons/basics/date-time/)
3. [Timex Library Documentation](https://hexdocs.pm/timex/readme.html)
4. [Erlang DateTime Docs](https://erlang.org/doc/apps/stdlib/time.html)
5. [Why programming languages have their own date/time libraries](https://www.toptal.com/software/why-do-programming-languages-have-their-own-date-time-libs)

Enjoy date calculation on your Elixir journey!