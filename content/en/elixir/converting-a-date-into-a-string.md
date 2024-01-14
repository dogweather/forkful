---
title:                "Elixir recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Date conversion is a common task in programming, and as an Elixir developer, you may find yourself needing to convert a date into a string at some point. This can be useful for displaying dates in a certain format or for saving them in a database. Regardless of the reason, knowing how to convert a date into a string is a valuable skill to have in your toolkit.

## How To

The process of converting a date into a string in Elixir involves using the `Date` and `String` modules. Let's take a look at some examples:

```
Elixir> Date.utc_today() |> String.to_date()
"2021-10-20"

Elixir> Date.utc_today() |> String.to_date("dd/mm/yyyy")
"20/10/2021"
```

In the first example, we use the `Date.utc_today()` function to get the current date in the UTC format. Then, we use the `String.to_date()` function to convert this date into a string. The output is the date in the standard ISO format of `yyyy-mm-dd`.

In the second example, we pass the desired format as an argument to the `String.to_date()` function. In this case, we want the date to be converted into the `dd/mm/yyyy` format. The output is the same date, but in the desired format.

You can also use the `~D` sigil to convert a date into a string without needing to use the `String` module. Here is an example:

```
Elixir> ~D[2021-10-20]
"2021-10-20"
```

## Deep Dive

Behind the scenes, the `String.to_date()` function uses the `ISO` module from the `Calendar` library to convert dates into strings. This module handles formatting dates according to ISO 8601, the international standard for date and time representation.

It is important to note that when converting dates, Elixir uses the Gregorian calendar system by default. However, you can specify a different calendar system by passing a reference to the `Calendar` module as an argument to the `String.to_date()` function.

For example, if you want to convert a date into a string using the Julian calendar, you could do this:

```
Elixir> Date.utc_today() |> String.to_date("dd/mm/yyyy", Calendar.Julian)
"07/10/2021"
```

## See Also

For more information on date and time functions in Elixir, check out the official documentation: 
- https://hexdocs.pm/elixir/Calendar.html
- https://hexdocs.pm/elixir/Calendar.ISO.html
- https://hexdocs.pm/elixir/String.html#to_date/2