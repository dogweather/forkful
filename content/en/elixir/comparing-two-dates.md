---
title:    "Elixir recipe: Comparing two dates"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing two dates may seem like a simple task at first, but it can actually be quite challenging depending on the programming language you are using. In Elixir, knowing how to effectively compare dates can save you time and headaches when working with date-related logic in your code.

## How To

To compare two dates in Elixir, we can use the `DateTime.compare/2` function. This function takes in two DateTime values and returns -1, 0, or 1, depending on whether the first date is earlier, equal, or later than the second date.

Let's take a look at an example:

```Elixir
date1 = DateTime.from_iso8601("2021-01-01T00:00:00.000Z")
date2 = DateTime.from_iso8601("2021-01-02T00:00:00.000Z")

DateTime.compare(date1, date2)
# Output: -1
```

In this example, `date1` is earlier than `date2`, so the `DateTime.compare` function returns -1. But what if the two dates are the same? Let's see:

```Elixir
date3 = DateTime.from_iso8601("2021-01-01T00:00:00.000Z")

DateTime.compare(date1, date3)
# Output: 0
```

Since both dates are the same, the `DateTime.compare` function returns 0. This can be useful in scenarios where you need to check if two dates are equal.

But what about comparing times within a date? For that, we can use the `DateTime.compare/3` function. This function takes in three arguments: the first date, the second date, and a unit of time (such as `:hours`, `:minutes`, etc.).

Let's see an example:

```Elixir
date4 = DateTime.from_iso8601("2021-01-01T12:00:00.000Z")

DateTime.compare(date1, date4, :hours)
# Output: -12
```

In this example, the `DateTime.compare` function is comparing the two dates by hours and returns -12, since `date1` is 12 hours earlier than `date4`.

## Deep Dive

When it comes to comparing two dates in Elixir, there are a few important things to keep in mind.

First, always make sure to use `DateTime` values for accurate comparisons. While Elixir does have a `Date` module, it only deals with dates and does not include time information.

Secondly, when using the `DateTime.compare/3` function, be aware that it compares by the specified unit of time and not just the total time difference. In other words, comparing two dates by minutes will return a different result than comparing by hours, even if the total time difference is the same.

## See Also

- Elixir `DateTime` module documentation: https://hexdocs.pm/elixir/DateTime.html
- Elixir `Date` module documentation: https://hexdocs.pm/elixir/Date.html
- Elixir `DateTime` built-in functions: https://hexdocs.pm/elixir/DateTime.html#functions