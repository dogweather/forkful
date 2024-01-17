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

Getting the current date is the process of retrieving the current date and time from your computer's system clock. It is a common task for programmers who want to keep track of time-sensitive events or display the current date and time in their applications.

## How to:

```Elixir
Date.utc_today()
#=> ~D[2021-09-29]

Date.utc_today() |> Date.to_string()
#=> "2021-09-29"
```

The `Date.utc_today()` function returns the current date in the UTC format. The resulting date is represented using the `~D[YYYY-MM-DD]` syntax. To convert it into a string, we can use the `Date.to_string()` function.

```Elixir
Date.to_string(Date.utc_today(), [{:format, "{ISO}"}, {:width, 1}])
#=> "{2021-09-29T00:00:00.000Z}"

Date.to_string(Date.utc_today(), [{:format, "{ISO8601}"}, {:width, 1}])
#=> "{2021-09-29T00:00:00Z}"

Date.strftime(Date.utc_today(), "%m/%d/%Y")
#=> "09/29/2021"
```

If we want to customize the format of the date, we can use the `Date.to_string()` or the `Date.strftime()` functions. The `Date.to_string()` function accepts various options like `{:format, "{ISO8601}"} or {:width, 1}` to specify the output format. On the other hand, `Date.strftime()` uses the [strftime](https://man7.org/linux/man-pages/man3/strftime.3.html) format specifiers to define the output format.

## Deep Dive:

In Elixir, dates and times are represented using the `Date` and `Time` modules. The `Date` module provides functions for manipulating dates, while the `Time` module handles time-related functions.

Alternative methods for getting the current date in Elixir include using the built-in `:os.timestamp()` function or the [Timex](https://github.com/bitwalker/timex) library. These methods offer additional features such as timezone handling and date manipulation.

Under the hood, Elixir uses the `erlang:now()` function to get the current date and time from the Erlang VM. The `Date.utc_today()` function then converts the Erlang timestamp into a `Date` struct. This approach ensures consistent date and time across different systems and platforms.

## See Also:

- [Elixir Date module documentation](https://hexdocs.pm/elixir/Date.html)
- [Elixir Time module documentation](https://hexdocs.pm/elixir/Time.html)
- [Erlang :os.timestamp() function documentation](https://erlang.org/doc/man/os.html#timestamp-0)
- [Timex library documentation](https://hexdocs.pm/timex/index.html)