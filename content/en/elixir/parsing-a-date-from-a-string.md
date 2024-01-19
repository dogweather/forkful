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

# Parsing a Date from a String: An Elixir Guide

## What & Why?

Parsing a date from a string is about transforming a string, e.g. "2020-12-30", into a structured Date object. Programmers do this to process, manipulate, and use date data in their code in a more convenient and accurate manner.

## How to:

Let's get straight to how you can get this done in Elixir.

```elixir
iex> {:ok, date} = Date.from_iso8601("2022-05-14")
{:ok, ~D[2022-05-14]}

iex> date
~D[2022-05-14]
```

`Date.from_iso8601/1` is a built-in function in Elixir that transforms an ISO8601 date string into a date struct.

Notice when the string isn't a valid ISO8601 date, it'll return an error tuple:

```elixir
iex> {:error, reason} = Date.from_iso8601("Not a date")
{:error, :invalid_format}
```

## Deep Dive

Elixir's approach to date parsing is inspired by the Erlang/OTP's robustness principle - programs should be "liberal in what they accept and conservative in what they send". Using simple and explicit tuple forms, `{:ok, result}` or `{:error, reason}` for results is a distinctive and robust feature in Elixir.

You have alternatives for parsing dates like using the `Timex` library if you're dealing with complex date formats. However, for standard ISO8601 dates, Elixir's built-in `Date.from_iso8601/1` function is sufficient.

Parsing dates from strings in Elixir is straightforward but remember it depends heavily on the input data being in the correct format. Always validate your input string before using it.

## See Also

- Elixir's [official documentation on Date](https://hexdocs.pm/elixir/Date.html)
- [Robustness Principle](https://en.wikipedia.org/wiki/Robustness_principle)
- [Parsing Dates with Timex in Elixir](https://hexdocs.pm/timex/Timex.html#parse/2)