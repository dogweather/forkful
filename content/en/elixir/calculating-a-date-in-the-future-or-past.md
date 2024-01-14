---
title:    "Elixir recipe: Calculating a date in the future or past"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why
Calculating dates in Elixir can be a useful tool for any programmer. Whether you need to schedule tasks, track events, or simply keep track of time, knowing how to calculate a date in the future or past can greatly improve your coding efficiency.

## How To
To calculate a date in the future or past in Elixir, we can use the `Calendar` module. This module provides us with functions to manipulate dates, times, and time zones. Let's look at some examples of how to use these functions.

```Elixir
# Calculate a date 7 days from now
Calendar.DateTime.add(DateTime.utc_now(), 7, "days")
# output: ~U[2020-04-30 07:09:31Z]

# Calculate a date 1 year from now
Calendar.DateTime.add(DateTime.utc_now(), 1, "year")
# output: ~U[2021-04-23 07:09:31Z]

# Calculate a date 6 months ago
Calendar.DateTime.add(DateTime.utc_now(), -6, "months")
# output: ~U[2019-10-23 07:09:31Z]

# Calculate the number of days between two dates
date1 = ~D[2020-04-23]
date2 = ~D[2020-05-01]
days = Calendar.Date.diff(date2, date1, :day)
# output: 8
```

As we can see, the `Calendar` module allows us to easily add or subtract time from a given date. We can specify the unit of time (days, months, years) and it will give us the corresponding date. We can also use the `Calendar.Date.diff` function to calculate the difference in days between two dates.

## Deep Dive
Under the hood, the `Calendar` module uses the Erlang `:calendar` module to handle date and time calculations. The `DateTime` type in Elixir is actually an Erlang tuple with some additional functionality. This allows for efficient and accurate calculations while also giving us the convenience of Elixir's syntax.

It is important to note that the `Calendar` module only supports dates from 4714 BC to 5899 AD. This is due to limitations in the underlying Erlang library. However, this range covers most use cases for date calculations.

## See Also
- Elixir official documentation on `Calendar`: https://hexdocs.pm/elixir/Calendar.html
- Erlang official documentation on `:calendar`: http://erlang.org/doc/man/calendar.html
- Elixir School's lesson on `Calendar`: https://elixirschool.com/en/lessons/basics/calendar/