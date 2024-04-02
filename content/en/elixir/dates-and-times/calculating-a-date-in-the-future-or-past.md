---
date: 2024-01-20 17:30:50.638459-07:00
description: "Figuring out a future or past date involves computing a date that is\
  \ so many days, months, or years from a specific point in time. Programmers often\
  \ need\u2026"
lastmod: '2024-03-13T22:44:59.794379-06:00'
model: gpt-4-1106-preview
summary: "Figuring out a future or past date involves computing a date that is so\
  \ many days, months, or years from a specific point in time. Programmers often need\u2026"
title: Calculating a date in the future or past
weight: 26
---

## What & Why?
Figuring out a future or past date involves computing a date that is so many days, months, or years from a specific point in time. Programmers often need this to track events, schedule jobs, or handle expiry dates.

## How to:
Using Elixir's built-in `Date` module, you can easily play with the timeline.

```elixir
# Add to or subtract from a given date
date_today = ~D[2023-04-15]
{year, month, day} = date_today

# Calculate a date 10 days in the future
date_future = Date.add(date_today, 10)
IO.inspect(date_future)  # => ~D[2023-04-25]

# Calculate a date 30 days in the past
date_past = Date.add(date_today, -30)
IO.inspect(date_past)  # => ~D[2023-03-16]
```

Notice how `Date.add/2` simply takes the number of days you want to travel in the time continuum.

## Deep Dive
The ability to compute dates in the future or past isn't new. Historical programming languages also had their ways—think COBOL or FORTRAN. However, Elixir brings functional flair and the immutability of data to the table, making date calculations straightforward and less prone to errors.

Alternatives? You could manually calculate by adding seconds, minutes, and so on, but why reinvent the wheel when Elixir provides a robust `Date` module? Especially considering time-based calculations can get complex, accounting for leap years, time zones, and daylight saving changes.

Implementation details revolve around understanding Elixir's `:calendar` module and the underlying Erlang implementations. We're standing on the shoulders of eras of date and time functionality evolution, with Elixir's syntax sugar making it all the sweeter.

## See Also
- Elixir's official `Date` module documentation: https://hexdocs.pm/elixir/Date.html
- "Date, Time, and Time Zones in Elixir": An article exploring Elixir's time-handling capabilities in depth.
- Erlang's `:calendar` module documentation: http://erlang.org/doc/apps/erts/calendar.html
