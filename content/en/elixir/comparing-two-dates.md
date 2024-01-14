---
title:    "Elixir recipe: Comparing two dates"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

Comparing dates is a common task in programming, especially when working with events or scheduled tasks. By understanding how to compare dates efficiently, you can ensure that your code runs smoothly and accurately reflects the desired behavior.

## How To

To compare dates in Elixir, we can use the `DateTime` module, which provides various functions for working with dates and times.

First, we need to convert our dates into a `DateTime` struct. We can do this using the `DateTime.from_iso8601/1` function, which takes in a string representing the date in ISO 8601 format.

```Elixir
date1 = DateTime.from_iso8601("2021-10-10T10:00:00Z")
date2 = DateTime.from_iso8601("2021-10-11T10:00:00Z")
```

Next, we can use the `DateTime.compare/2` function to compare the two dates. This function returns `:lt` if the first date is earlier, `:gt` if the first date is later, and `:eq` if the two dates are equal.

```Elixir
DateTime.compare(date1, date2)
# Output: :lt
```

We can also use the `DateTime.diff/2` function to get the difference between two dates in a specific unit (days, hours, minutes, etc.). This function returns a `%DateTime.Duration{}` struct, which contains the difference between the two dates.

```Elixir
DateTime.diff(date1, date2, :days)
# Output: %DateTime.Duration{days: 1}
```

## Deep Dive

When comparing dates, it's important to understand how different timezones and daylight saving time (DST) can affect the results.

Elixir's `DateTime` module uses the UTC timezone by default. This means that when converting dates into `DateTime` structs, they will be in UTC time. However, the `DateTime.compare/2` function takes into account the timezone offset when comparing dates. So if our code is running in a different timezone, we may get unexpected results.

Additionally, DST can also affect how dates are compared. When a daylight saving time change occurs, a specific date and time may not exist or may be repeated. This can cause issues when comparing dates, as they may not be exactly one day apart. It's important to consider these scenarios when working with dates and ensure that your code accounts for them.

## See Also

- [Elixir DateTime Module Documentation](https://hexdocs.pm/elixir/DateTime.html)
- [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Daylight Saving Time](https://www.timeanddate.com/time/dst/)