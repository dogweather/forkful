---
title:    "Elixir recipe: Calculating a date in the future or past"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past can be a useful task for a variety of scenarios. From scheduling appointments and events to managing project timelines, being able to accurately determine a date in the future or past is an important skill for any programmer to have. In this blog post, we will explore how to do just that using the Elixir programming language.

## How To
To calculate a date in the future or past, we will be using the `Date` and `Timex` libraries in Elixir. First, we need to import these libraries by adding the following line to the top of our code:
```Elixir
import Date, only:[add: 2]
import Timex
```

To calculate a date in the future, we will use the `add` function from the `Date` library which takes two arguments - the starting date and the number of days to add. For example, if we want to calculate the date 30 days from today, we can use the following code:
```Elixir
Date.now() |> add(30)
```
The output will be in the format of `%Date{}` which contains the year, month, and day of the calculated date.

Similarly, to calculate a date in the past, we can use the same `add` function but with a negative number of days. For example, if we want to calculate the date 10 days before today, we can use the following code:
```Elixir
Date.now() |> add(-10)
```

We can also specify a specific date as the starting point by passing in the year, month, and day as arguments to the `Date.new` function. For example, to calculate the date 20 days from January 1, 2021, we can use the code:
```Elixir
Date.new(2021, 1, 1) |> add(20)
```

## Deep Dive
Internally, Elixir uses the Erlang Date library which represents dates as the number of days since January 1, 1970. This allows for efficient calculations of dates in the future or past without relying on complex algorithms. The `Timex` library provides additional functions for manipulating dates, such as calculating the difference between two dates or finding the day of the week for a given date.

It's also worth noting that the `Date` library does not handle timezones, so if your application requires timezone awareness, you may need to use the `DateTime` library instead.

## See Also
- [Date - Elixir Documentation](https://hexdocs.pm/elixir/Date.html)
- [Timex - Elixir Documentation](https://hexdocs.pm/timex/Timex.html)
- [DateTime - Elixir Documentation](https://hexdocs.pm/elixir/DateTime.html)