---
title:                "Parsing a date from a string"
html_title:           "Elixir recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string means converting that string into a meaningful date format that a computer understands. Programmers do it to handle different date formats and utilize them in their applications.

## How to:

In Elixir, the `DateTime` module comes packed with various functions to parse a date from a string. A function that can be used to achieve this task is `DateTime.from_iso8601/2`. This function parses a string in the format: "YYYY-MM-DD" or "YYYYMMDD". 

```Elixir
iex> DateTime.from_iso8601("2021-11-22T22:40:05.923678Z")
{:ok,
 %DateTime{
   year: 2021,
   month: 11,
   day: 22,
   hour: 22,
   minute: 40,
   second: 5,
   microsecond: {923678, 6},
   time_zone: "Etc/UTC",
   utc_offset: 0,
   std_offset: 0,
   zone_abbr: "UTC"
 }}
```

## Deep Dive

Historically, date parsing has always been a common task in many programming languages due to its importance in handling temporal data. The need for a universal date and time standard led to the creation of the ISO 8601 format, which is the default format for `DateTime.from_iso8601/2`.

However, if your date string isn't in the ISO 8601 format, Elixir provides alternative functions in the `DateTime` module like `DateTime.parse/2`. The `DateTime.parse/2` function takes a date string and a formatter to parse the string into DateTime.

Under the hood, `DateTime.from_iso8601/2` is calling `Calendar.ISO.from_iso8601/1` in the Elixir codebase. It is then validating that the date is a valid Gregorian calendar date before converting and returning it.

```Elixir
iex> DateTime.parse("13-01-2021 09:00", "{0D}-{0M}-{0Y} {0h}:{0m}")
{:ok, ~U[2021-01-07T09:00:00Z]}
```

## See Also

For a more detailed dive into Elixir Date and Time, check out the `DateTime` module in the Elixir [official doc](https://hexdocs.pm/elixir/DateTime.html). You can also check out other Elixir date parsing libraries like [Timex](https://hexdocs.pm/timex/readme.html) and [Calendar](https://hexdocs.pm/calendar/readme.html) for more date formatting and parsing options.