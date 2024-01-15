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

## Why

 Calculating dates in the future or past is a common task in programming, and it can be especially useful when working with time-sensitive data or creating scheduling systems.

## How To

To calculate a date in the future or past, Elixir provides the `Date` module which offers a variety of functions for manipulating dates. Let's take a look at a few examples:

```
# Calculate the date 1 week from today
Date.plus(Date.utc_today, 7, :days)
# 2019-07-17

# Calculate the date 2 years from a specific date
Date.plus(Date.from_erl({2020, 1, 1}, {0, 0, 0}), 2, :years)
# 2022-01-01

# Calculate the date 3 months before a specific date
Date.minus(Date.from_gregorian(2019, 7, 15), 3, :months)
# 2019-04-15
```

As you can see, the `plus/3` and `minus/3` functions take in a `Date` as the first argument, followed by the amount and unit of time to be added or subtracted.

You can also use the `add/2` and `subtract/2` functions to perform the same calculations, but instead of specifying a unit of time, you pass in a `DateTime.Interval` struct:

```
# Calculate the date 5 days from now
DateTime.add(Date.utc_today, %DateTime.Interval{days: 5})
# 2019-07-16

# Calculate the date 10 hours from a specific date
stamp = DateTime.from_erl({2019, 7, 10}, {13, 0, 0})
DateTime.add(stamp, %DateTime.Interval{hours: 10})
# 2019-07-11T23:00:00Z
```

One useful function for calculating dates is the `diff/2` function, which returns the difference between two dates in the specified unit of time:

```
# Calculate the difference between two dates in weeks
Date.diff(Date.utc_today, Date.from_gregorian(2019, 7, 1), :weeks)
# 2
```

## Deep Dive

Under the hood, Elixir uses the "Erlang Calendar Module" to perform all date calculations. Elixir's `Date` module simply provides a more user-friendly API for working with dates.

It's worth noting that dates in Elixir are represented as a tuple in the format `{year, month, day}` and times are represented as a tuple in the format `{year, month, day, hour, minute, second}`. This allows for easy conversion between `Date` and `DateTime` formats.

Also, worth mentioning is that Elixir uses UTC time by default, but you can use the `DateTime` module to convert to other timezones if needed.

## See Also

- [Elixir Date Module Documentation](https://hexdocs.pm/elixir/Date.html)
- [DateTime Module Documentation](https://hexdocs.pm/elixir/DateTime.html)
- [Erlang Calendar Module Documentation](http://erlang.org/doc/man/calendar.html)