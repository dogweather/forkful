---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing Dates from Strings in Elixir

## What & Why?

Parsing a date from a string is simply converting a readable date-time written in the form of a text string into a standard Date-Time format that a computer can understand and manipulate. Programmers do this for tasks such as sorting dates, calculating time intervals, or formatting dates in different styles.

## How To:

Here's how you can use Elixir's built-in `Date` module to parse a date string:

```Elixir
iex> {:ok, date} = Date.from_iso8601("2022-01-01")
{:ok, ~D[2022-01-01]}
```

In this example above, we're parsing an ISO8601 formatted date string. If the string is not a valid ISO8601 date, `Date.from_iso8601/1` will return `{:error, :invalid_format}`.

```Elixir
iex> Date.from_iso8601("2022/01/01")
{:error, :invalid_format}
```

If parsing of the date string was successful, then you can use the date in further operations. For instance:

```Elixir
iex> date.day
1
```

This will return the day of the parsed date.

## Deep Dive

Historically, date parsing was not always straightforward in Elixir because of the lack of built-in functions. Developers had to rely on external libraries like Timex. However, with the introduction of the `Date` module in Elixir 1.3, such tasks became much simpler.

There are alternatives to `Date.from_iso8601/1` like `Date.new/3` where you pass the year, month, and day as arguments to get a `Date` struct. It's more cumbersome to use, but it gives you more control.

As for the implementation, `Date.from_iso8601/1` first checks the format of the date string. If it's valid, the function separates the year, month, and day components and constructs a `Date` struct. If the format is invalid, it returns `{:error, :invalid_format}`.

## See Also 

- [The Elixir `Date` module documentation](https://hexdocs.pm/elixir/Date.html)
- [Mastering DateTime in Elixir](https://medium.com/@cblavier/mastering-datetime-in-elixir-1-elixir-datetime-basics-b9307d0b2fb2)