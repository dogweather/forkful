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

## Why
Comparing two dates is a common task in programming, especially when working with time-sensitive data or implementing scheduling features. It allows developers to check if a date comes before, after, or is equal to another date, and make decisions based on that comparison.

## How To
To compare two dates in Elixir, we can use the `:calendar.compare/2` function from the `Calendar` module. Let's take a look at some examples:

```Elixir
date1 = ~D[2021-10-01]
date2 = ~D[2021-10-15]

Calendar.compare(date1, date2)
# => :lt (date1 is less than date2)

date3 = ~D[2021-10-15]

Calendar.compare(date2, date3)
# => :eq (date2 is equal to date3)

date4 = ~D[2021-09-15]

Calendar.compare(date3, date4)
# => :gt (date3 is greater than date4)
```

We can also use the `:calendar.date_comp/2` function, which returns an integer indicating the difference in days between two dates:

```Elixir
date1 = ~D[2021-10-01]
date2 = ~D[2021-10-15]

Calendar.date_comp(date1, date2)
# => -14
```

Note that the first argument must be the earlier date and the second argument must be the later date.

## Deep Dive
Behind the scenes, the `:calendar.compare/2` function uses the Erlang library `:calendar` to perform the comparison. This library uses the proleptic Gregorian calendar, meaning it extends the Gregorian calendar to dates before its introduction in 1582.

If you need to compare dates in different time zones, you can use the `:calendar.datetime_to_gregorian_seconds/3` function to convert the dates to a standardized form before comparing them. This function takes three arguments: the date in question, a timezone offset in seconds, and a timezone string.

## See Also
- `Calendar.compare/2` documentation: https://hexdocs.pm/elixir/Calendar.html#compare/2
- `Calendar.date_comp/2` documentation: https://hexdocs.pm/elixir/Calendar.html#date_comp/2
- `:calendar` library documentation: https://erlang.org/doc/man/calendar.html
- Proleptic Gregorian calendar: https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar