---
title:                "Parsing a date from a string"
aliases:
- /en/elixir/parsing-a-date-from-a-string/
date:                  2024-02-03T19:02:44.156330-07:00
model:                 gpt-4-0125-preview
simple_title:         "Parsing a date from a string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string in Elixir involves converting text representations of dates and times into Elixir's native date structures, allowing for more flexible manipulation, comparison, and formatting of date and time values. Programmers perform this task to handle user input, process data from external sources, or interact with APIs that communicate dates and times as strings.

## How to:
Elixir's standard library, combined with the power of the Erlang `:calendar` module, provides basic support for parsing dates. For more complex needs, the `Timex` library is a popular choice, offering extensive features for working with dates, times, and timezones.

### Using Elixir's Standard Library
```elixir
date_string = "2023-04-21"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)  # => ~D[2023-04-21]
```

To parse a datetime string including time zone information, you might directly utilize Erlang functions, given that direct timezone handling isn't part of Elixir's standard Date module.
```elixir
datetime_string = "2023-04-21T15:30:00Z"
{:ok, datetime, 0} = DateTime.from_iso8601(datetime_string)
IO.inspect(datetime)  # => #DateTime<2023-04-21 15:30:00Z>
```

### Using Timex
First, add `Timex` to your mix.exs dependencies:
```elixir
def deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```
Then, run `mix deps.get` to fetch the new dependency.

Here is how you can use Timex to parse a date from a string:
```elixir
import Timex

date_string = "April 21, 2023"
date = Timex.parse!(date_string, "{Mfull} {D}, {YYYY}")
IO.inspect(date)  # => ~N[2023-04-21 00:00:00]
```

Timex allows parsing various formats and even supports natural language dates, making it highly flexible for parsing dates and times from user input or external data sources.
