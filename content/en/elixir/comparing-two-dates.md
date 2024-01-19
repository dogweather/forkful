---
title:                "Comparing two dates"
html_title:           "Elixir recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates means measuring the difference between them in terms of days, minutes, seconds, etc. Programmers do it to execute certain actions based on date-time conditions like scheduling tasks, creating reminders, and calculating durations.

## How to:

In Elixir, we use `DateTime` module for comparing two dates. Let's look at some code examples:

```elixir
# Create two DateTime instances
date1 = DateTime.utc_now()
date2 = DateTime.add(date1, 3600)

# Compare two dates
DateTime.compare(date1, date2)
```
The 'DateTime.compare' function returns one of three atoms: `:lt` (less than), `:eq` (equal), or `:gt` (greater than) meaning date1 is less than, equal to, or greater than date2 respectively.

## Deep Dive

Elixir (beginning from version 1.3) introduced the `DateTime` module for handling and comparing dates. Before that, the Elixir community relied heavily on third-party libraries such as Timex.

An alternative approach to comparing two dates is using comparison operators (>, <, ==, etc.), but DateTime.compare is safer as it guards against invalid comparisons.

In Elixir, dates are compared based on the Gregorian calendar. This can sometimes lead to unexpected results when dealing with other calendar systems. So, stick with the Gregorian calendar for all date comparisons to keep things simple.

## See Also

For more on Elixir date comparisons and handling, check out these links:
- [DateTime Module (official docs)](https://hexdocs.pm/elixir/DateTime.html)
- [Understanding Date and time in Elixir](https://alchemist.camp/episodes/elixir-dates-times)
- [Date comparison in Elixir (StackOverflow)](https://stackoverflow.com/questions/37679468/how-do-i-compare-two-dates-in-elixir)