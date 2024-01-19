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

## What & Why?

Getting the current date is pretty self-explanatory - it's a process of obtaining today's date from the system. Programmers usually need it to log events, timestamp records, or to handle date-specific functions in applications.

## How To:

### Getting current date in Elixir:
In Elixir, you can obtain the current date using the `Date.utc_today/0` function from the `Date` module. Here's how you do it:

```Elixir
Date.utc_today()
```
To demonstrate, let's run it in Elixir's interactive shell (IEx):

```Elixir
iex> Date.utc_today()
~D[2022-03-18]
```
In this example, the output is `~D[2022-03-18]`, which represents March 18, 2022, in UTC timezone (replace with current date when used as a sample).

## Deep Dive

### Historical Context:
The `Date.utc_today/0` function in Elixir is built upon Erlang, Elixir's underlying language. Erlang provides fantastic support for native date and time handling, which Elixir leverages.

### Alternatives:
Elixir also offers alternatives to get current date:
- `NaiveDateTime.utc_now/0`: This method returns both the current date and time.
- `DateTime.utc_now/0`: Similar to `NaiveDateTime.utc_now/0`, but this one also includes timezone information.

### Implementation Details:
`Date.utc_today/0` returns a `Date` struct which represents a date keeping year, month, and day fields only. It's important to note that it does not consider timezone, and always returns the date in UTC.

## See Also:

To delve deeper into Elixir's date and time manipulation, take a look at these:
1. Elixir's official docs about `Date` module: [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
2. Elixir School's guide to Date & Time: [https://elixirschool.com/en/lessons/basics/date-time/](https://elixirschool.com/en/lessons/basics/date-time/)
3. Erlang's official docs on time module: [https://erlang.org/doc/man/time.html](https://erlang.org/doc/man/time.html)