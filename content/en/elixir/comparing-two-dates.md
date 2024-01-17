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
Comparing two dates is a common programming task that involves determining the relationship between two given dates, such as which one is earlier or later. Programmers do this to perform various date-based operations, such as scheduling tasks, sorting data, or calculating time differences.

## How to:
To compare two dates in Elixir, we can use the `Calendar.compare/2` function. This function takes two date values and returns -1, 0, or 1 depending on the relationship between the two dates. Let's see some examples:

```Elixir
iex> Calendar.compare({2020, 5, 15}, {2021, 1, 1})
-1
```

In the above code, we are comparing May 15, 2020 to January 1, 2021, and since 2020 comes before 2021, the result is -1.

We can also use the `DateTime.compare/2` function, which works the same way but for comparing date and time values:

```Elixir
iex> DateTime.compare({2020, 5, 15, 13, 30, 0}, {2020, 5, 15, 12, 0, 0})
-1
```

Here, we are comparing May 15, 2020 at 1:30 PM to May 15, 2020 at 12:00 PM, and once again, the earlier time comes first, resulting in -1.

## Deep Dive:
In Elixir, dates are represented as tuples in the form `{year, month, day}` and times as tuples in the form `{year, month, day, hour, minute, second}`. Elixir's `Calendar` module provides various functions for working with dates, including comparing them.

An alternative to using Elixir's built-in functions would be to use the `Calendar.Date` module, which offers a `diff/2` function for comparing dates and returning the difference in days, similar to the `DateTime.diff/2` function for comparing date and time values.

The compare functions in Elixir also take into account timezones and daylight savings time, making them reliable when working with different timezones.

## See Also:
- [Elixir Date and Time module documentation](https://hexdocs.pm/elixir/Calendar.html)
- [Comparison operators in Elixir](https://elixir-lang.org/getting-started/comparison-operators.html)
- [Working with dates and times in Elixir](https://code.tutsplus.com/tutorials/working-with-dates-and-times-in-elixir--cms-34257)