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

## What & Why?

Calculating a date in the future or past is the process of determining a date that occurs either before or after a given date by a specified amount of time. Programmers often do this in order to schedule events or tasks, determine deadlines, or track the lifespan of an object.

## How to:

Calculating a date in the future or past in Elixir is made easy with the `Calendar` module. First, we need to import the module with `import Calendar`. Then we can use the `add/2` function to calculate a future date by providing the start date and the time interval in seconds. Similarly, the `sub/2` function can be used to calculate a past date.

```Elixir
import Calendar

future_date = Calendar.add({2020, 12, 31}, 86400)
#=> {{2021, 1, 1}, {0, 0, 0}}

past_date = Calendar.sub({2020, 12, 31}, 86400)
#=> {{2020, 12, 30}, {0, 0, 0}}
```

## Deep Dive:

Calculating dates in the future or past has been a common programming task for a long time. In Elixir, the `Calendar` module was introduced in version 1.3 as a more modern and cleaner approach to date and time calculations compared to the existing `:calendar` module. The module uses the DateTime struct, which contains the date and time in a convenient format.

An alternative to using the `Calendar` module is the `:calendar` module, which provides many functions for performing date and time calculations as well. However, some developers prefer to use the newer `Calendar` module due to its cleaner syntax.

The `add/2` and `sub/2` functions in the `Calendar` module use the Erlang BIF (Built-In Function) `calendar:time_add/2` and `calendar:time_sub/2` underneath, which perform the actual date calculation. This ensures accurate and efficient date calculation in Elixir.

## See Also:

- [Official Elixir Documentation for Calendar Module](https://hexdocs.pm/elixir/Calendar.html)
- [Blog post on why Elixir's Calendar module is better than :calendar](https://thinkingelixir.com/valid-timezones-elixir-1.3/)
- [Erlang Calendar module BIFs documentation](http://erlang.org/doc/man/calendar.html)