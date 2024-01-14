---
title:                "Elixir recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past can be a useful tool for various tasks such as scheduling events, implementing countdowns, or organizing data based on date ranges. With Elixir, this process is made even easier and more efficient through its powerful date and time manipulation functions.

## How To

To calculate a date in the future or past, we first need to import the `DateTime` module in Elixir. This can be done by typing the following code in an Elixir shell or by including it in your Elixir project file.

```Elixir
import DateTime
```

Next, we need to specify a starting date and time. This can be done by creating a `DateTime` struct using the `DateTime.from_naive/2` function and passing in the desired starting date and time in the format of `{year, month, day, hour, minute, second}`. For example:

```Elixir
starting_date = DateTime.from_naive({2021, 9, 1, 12, 0, 0}, "Etc/UTC")
```

We can then use the `DateTime.add/3` function to add or subtract a certain number of days, hours, minutes, or seconds to our starting date. For example, if we want to calculate a date 3 days in the future, we can do so by passing in `3` as the second argument and `:days` as the third argument, along with our starting date as the first argument. The resulting date will be returned as a `DateTime` struct.

```Elixir
DateTime.add(starting_date, 3, :days)
```

The output would look something like this:

```Elixir
%DateTime{calendar: Calendar.ISO, day: 4, hour: 12, microsecond: {0, 0}, minute: 0, month: 9, second: 0, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0, year: 2021}
```

Similarly, if we want to calculate a date 2 hours and 30 minutes into the future, we can do so by passing in `2` and `30` as the second and third arguments respectively, along with `:hours` as the third argument. The output would look like this:

```Elixir
%DateTime{calendar: Calendar.ISO, day: 1, hour: 14, microsecond: {0, 0}, minute: 30, month: 9, second: 0, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0, year: 2021}
```

We can also calculate dates in the past by passing in a negative number as the second argument of `DateTime.add/3`.

## Deep Dive

Elixir's `DateTime` module contains many useful functions for manipulating dates and times. For example, we can use `DateTime.diff/2` to calculate the difference between two dates in terms of days, hours, minutes, or seconds.

```Elixir
starting_date = DateTime.from_naive({2021, 9, 1, 12, 0, 0}, "Etc/UTC")
another_date = DateTime.from_naive({2021, 8, 25, 8, 0, 0}, "Etc/UTC")

DateTime.diff(starting_date, another_date, :days)
```

The output would be `7`, as there are 7 days between the two dates.

We can also use `DateTime.equal?/2` to check if two dates are equal, taking into consideration the date, time, and time zone.

```Elixir
DateTime.equal?(starting_date, another_date)
```

This would return `false` since the two dates have different days and hours.

For more in-depth examples of date and time manipulation in Elixir, check out the official documentation for the `DateTime` module.

## See Also
- Official Elixir DateTime Module Documentation: https://hexdocs.pm/elixir/DateTime.html
- Date and Time Manipulation in Elixir by Andrea Leopardi: https://andrea.leopardi.it/2016/06/23/date-and-time-manipulation-in-elixir.html
- How to Work with Time in Elixir by Vaidehi Joshi: https://open.nytimes.com/how-to-work-with-time-in-elixir-14593830daa6