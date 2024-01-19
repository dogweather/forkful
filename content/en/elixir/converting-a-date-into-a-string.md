---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

In Elixir programming, converting a date to a string means changing the data format from a Date data structure to a readable string format. Programmers often do this to make dates easier for humans to read and understand, or to serialize dates for storage in databases or transmission between systems.

## How to:

Here's a basic example of converting a date to a string in Elixir:

```Elixir 
date = Date.utc_today()
date_string = to_string(date)
IO.puts(date_string)
```

This script gets today's date, converts it to a string, and then outputs it. Here's what typical output might look like:

```Elixir
"2022-04-01"
```

## Deep Dive

Looking into Elixir's past, you can notice a commitment to flexible manipulation of data formats. José Valim, the creator of Elixir, worked on the Rails core team, where different ways of representing dates were crucial. This informs the flexibility of the `Date` module in Elixir.

Elixir has different ways or alternatives of converting a date to a string. For instance, you can use `Date.to_string/1`, or `NaiveDateTime.to_string/1` depending on your needs. These cater to naive datetimes, where you don’t consider timezones, or normal date.

When converting a Date struct to a string in Elixir, the `to_string/1` function relies on the Erlang Runtime System. It’s one of the ways Elixir leverages the power of the underlying Erlang system. It uses Erlang's `:calendar` module (from OTP 20 and onwards) to handle the conversion, producing an ISO 8601-compliant string.

## See Also:

1. Official Elixir `Date` documentation: https://hexdocs.pm/elixir/Date.html
2. Erlang's `:calendar` module: http://erlang.org/doc/man/calendar.html
3. José Valim in depth discussion about dates and calendars in Elixir: https://www2.erlang-solutions.com/elixirconf6
4. 'Learn Elixir' guide on data manipulation: https://www.learn-elixir.dev/#data-manipulation