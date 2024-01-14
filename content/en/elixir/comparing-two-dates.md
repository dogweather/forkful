---
title:                "Elixir recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
Date comparisons are a common task in programming, especially when dealing with time-sensitive data or events. In the Elixir programming language, there are various methods for comparing two dates. Understanding how to properly compare dates can help ensure accurate and efficient code.

## How To
In Elixir, dates are represented as tuples in the format `{year, month, day}`. To compare two dates, we can use the `:calendar.compare/2` function.

```
Elixir
date1 = {2021, 10, 15}
date2 = {2021, 10, 25}

:calendar.compare(date1, date2)
```

The output of this comparison will be `-1`, `0`, or `1`, indicating if `date1` is before, equal to, or after `date2`, respectively.

We can also use the `:calendar.datetime_to_gregorian_seconds/1` function to convert a date tuple into seconds, making it easier to compare two dates.

```
Elixir
date1 = {2021, 10, 15}
date2 = {2021, 10, 25}

seconds1 = :calendar.datetime_to_gregorian_seconds(date1)
seconds2 = :calendar.datetime_to_gregorian_seconds(date2)

if seconds1 < seconds2 do
  IO.puts "date1 is before date2"
elsif seconds1 > seconds2 do
  IO.puts "date1 is after date2"
else
  IO.puts "date1 is equal to date2"
end
```

The output of this code will be "date1 is before date2".

## Deep Dive
When comparing dates, it is important to consider time zones and daylight saving time. Elixir's `:calendar` module provides functions for dealing with these complexities.

For example, the `:calendar.local_time_to_universal_time` function converts a datetime tuple into a standardized universal time. This ensures that date comparisons are accurate regardless of time zone differences.

```
Elixir
local_time = {2021, 10, 31, 1, 30, 0}

universal_time = :calendar.local_time_to_universal_time(local_time)
```

The output of this code will vary depending on the local time zone, but the resulting `universal_time` will be adjusted accordingly.

## See Also
- Elixir documentation on date comparisons: https://hexdocs.pm/elixir/Calendar.html#compare/2
- Elixir date and time functions: https://hexdocs.pm/elixir/Calendar.html#module-date-and-time-functions