---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:35:23.042826-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/parsing-a-date-from-a-string.md"
changelog:
  2024-01-28, @dogweather, reviewed
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is about taking text, like "2023-04-05", and converting it to a date format your program can understand and work with. Programmers do this because dates come in plenty of formats, and they need consistency to compare, sort, or store them properly.

## How to:

In Elixir, you can parse dates using the `Date` module. Here's how to turn a string into a date:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

Sample output:

```elixir
~D[2023-04-05]
```

To handle different formats, you may use the `Timex` library:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

Sample output:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## Deep Dive

The `Date.from_iso8601/1` function is part of Elixir's standard library, introduced to ensure easy parsing of the ISO8601 date standard - a common date format. But life's not that simple; dates come in tons of formats. That's where `Timex`, a third-party Elixir library, comes into play. It's richer than the built-in Elixir date functions and helps handle a wide variety of date formats.

Elixir itself is immutable, which means parsed dates are no exception; they can't be changed once created. This feature ties back to the functional programming roots of Elixir, guaranteeing predictability and easier debugging.

Historically, date parsing has been tough due to varying standards. Yet with libraries like `Timex` and language features in Elixir, the complexity is abstracted away, making a developer's life a touch simpler.

## See Also

- [Elixir Date](https://hexdocs.pm/elixir/Date.html)
- [Timex Documentation](https://hexdocs.pm/timex/Timex.html)
- [ISO8601 Standard](https://www.iso.org/iso-8601-date-and-time-format.html)
