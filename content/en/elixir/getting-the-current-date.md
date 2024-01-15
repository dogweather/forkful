---
title:                "Getting the current date"
html_title:           "Elixir recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why 

Getting the current date is a common task in many programming projects. Whether it's for displaying the current date on a website or for tracking events, having the ability to access and manipulate the current date is crucial for many applications.

## How To 

To get the current date in Elixir, we can use the `Date` module from the `Calendar` library. This module provides several functions for working with dates, including getting the current date. Here's an example of how to retrieve the current date and format it as a string:

```elixir
current_date = Date.utc_today()
formatted_date = Date.format(current_date, "{YYYY}-{MM}-{DD}")
IO.puts(formatted_date)

# Output: 2021-12-28
```

We used the `utc_today` function to retrieve the current date, which returns a `Date` struct. Then, we used the `format` function to specify the format we wanted the date to be displayed in. In this case, we used `"{YYYY}-{MM}-{DD}"` to show the year, month, and day in a YYYY-MM-DD format. Lastly, we used `IO.puts` to print the formatted date to the console.

We can also get the current date and time by using the `DateTime` module from the `Calendar` library. Here's an example of how we can do that:

```elixir
current_datetime = DateTime.utc_now()
formatted_datetime = DateTime.format(current_datetime, "{YYYY}-{MM}-{DD} {hh}:{mm}:{ss}")
IO.puts(formatted_datetime)

# Output: 2021-12-28 15:28:42
```

Similarly to the previous example, we used the `utc_now` function to retrieve the current date and time as a `DateTime` struct. Then, we used the `format` function to specify the format we wanted the date and time to be displayed in. In this case, we used `"{YYYY}-{MM}-{DD} {hh}:{mm}:{ss}"` to show the year, month, day, hour, minute, and second in a YYYY-MM-DD hh:mm:ss format. Lastly, we used `IO.puts` to print the formatted date and time to the console.

## Deep Dive 

The `Date` and `DateTime` modules have several other functions that can be useful for working with dates and times. For example, we can use the `add` function to add or subtract a specified number of days, months, or years to a given date.

```elixir
current_date = Date.utc_today()
IO.inspect(current_date)

# Output: ~D[2021-12-28]

next_month = Date.add(current_date, 1, :month)
IO.inspect(next_month)

# Output: ~D[2022-01-28]

next_year = Date.add(current_date, 1, :year)
IO.inspect(next_year)

# Output: ~D[2022-12-28]
```

As we can see in the example, we can use `add` to add one month or one year to the current date, resulting in a new `Date` struct. This function can be handy when handling recurring events or creating date ranges.

## See Also 

Here are some additional resources to help you further explore working with dates and times in Elixir:

- [Elixir Date module documentation](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime module documentation](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Calendar library documentation](https://hexdocs.pm/elixir/Calendar.html)