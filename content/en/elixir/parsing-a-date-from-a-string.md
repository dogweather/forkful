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
Parsing a date from a string is the process of converting a date in string format to a more usable data structure. This is an important task for programmers as it allows them to manipulate and work with dates in various applications.

## How to:
```Elixir
# Using the built-in Date module:
iex> Date.from_iso8601("2021-01-01")
{:ok, ~D[2021-01-01]}

# Using the Timex library:
iex> {:ok, datetime} = Timex.parse("Jan 1, 2021", "{Month: MMM, Day: D, Year: YYYY}")
iex> datetime
~U[2021-01-01 00:00:00Z]
```

## Deep Dive:
Parsing dates has been a common task for programming languages since the early days. Elixir, being a functional language, offers a variety of options for date parsing. Apart from the built-in Date module, you can also use libraries like Timex or sweet_xml. These libraries not only handle date parsing but also offer additional features like timezone support and date formatting.

## See Also:
- [Date module documentation](https://hexdocs.pm/elixir/Date.html)
- [Timex library](https://hexdocs.pm/timex/Timex.html)
- [Sweet_xml library](https://hexdocs.pm/sweet_xml/SweetXml.html)