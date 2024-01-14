---
title:                "Elixir recipe: Converting a date into a string"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

As an emerging and highly versatile functional programming language, Elixir offers a wide range of utilities to make developers' lives easier. Converting data types and structures is a common task in any programming language, and in Elixir, one of the most frequent conversions is from a date to a string. In this blog post, we will explore why and how to convert a date into a string in Elixir.

## How To

To convert a date into a string in Elixir, we can use the `DateTime.to_string/2` function. This function takes two arguments - a date and a format string. The date can be provided in any of the following formats: `DateTime`, `Date`, `Ecto.DateTime`, `NaiveDateTime`, and `Timex.DateTime`. The format string determines the output format of the string representation of the date. Let's take a look at some examples:

```Elixir
# Converting DateTime to string
DateTime.to_string(~U[2021-10-13 20:15:35.000000], "{YYYY}-{0M}-{0D}T{HH}:{mm}:{ss}Z")
# Output: "2021-10-13T20:15:35Z"

# Converting Date to string
DateTime.to_string(~D[2021-10-13], "{0D}/{0M}/{YY}")
# Output: "13/10/21"

# Converting Ecto.DateTime to string
DateTime.to_string(%Ecto.DateTime{year: 2021, month: 10, day: 13}, "{0M}-{DD} {hh}:{mm}:{ss}")
# Output: "10-13 00:00:00"

# Converting NaiveDateTime to string
DateTime.to_string(~N[2021-10-13 20:15:35], "{DD}/{0M}/{YYYY} {mm}.{ss}")
# Output: "13/10/2021 15.35"

# Converting Timex.DateTime to string
DateTime.to_string(Timex.now, "{hh}-{0M}-{YY} {0D}")
# Output: "20-10-21 13"
```

In the above examples, we used a variety of date formats and output formats to showcase the flexibility of the `DateTime.to_string/2` function.

## Deep Dive

Behind the scenes, the `DateTime.to_string/2` function works by first converting the given date into an internal representation, a `:calendar.datetime` struct. This struct stores the date and time components in a tuple format, making it efficient for formatting. When a format string is provided, the function uses the `:calendar.format/2` function to convert the tuple representation into a string according to the specified format.

However, it is worth noting that the `DateTime.to_string/2` function only works with the `:calendar.datetime` struct. If a date in any other format is provided, the function will first convert it into a `:calendar.datetime` and then format it.

## See Also

Here are some additional resources for learning more about converting date into string in Elixir:

- Elixir `DateTime` module documentation: https://hexdocs.pm/elixir/DateTime.html
- Elixir `DateTime` cheatsheet: https://devhints.io/phoenix-datetime
- Elixir `:calendar` module documentation: https://hexdocs.pm/elixir/Calendar.html