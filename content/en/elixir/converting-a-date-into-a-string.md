---
title:    "Elixir recipe: Converting a date into a string"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to display a date in a certain format in your Elixir program? Converting a date into a string is a common task in programming, and in this blog post, we will explore how to do this using Elixir.

## How To

Converting a date into a string can be achieved using the `~D` sigil. This sigil formats a date according to the ISO 8601 standard. Let's take a look at an example:

```Elixir
~D[2021-07-22]
```

This sigil will output the date as a string in the format `yyyy-mm-dd`. In this case, the output will be `2021-07-22`.

You can also specify a different format using the `strftime` function. For example:

```Elixir
~D[2021-07-22] |> DateTime.to_erl |> :os.date("MM dd, yyyy")
```

This will output the date in the format `MM dd, yyyy`, which in this case would be `07 22, 2021`.

## Deep Dive

Behind the scenes, the `~D` sigil uses the `DateTime` module to convert the date into a string. This module provides several functions for working with dates and times in Elixir. The `to_erl` function is used to convert the Elixir date into an Erlang date, which is then passed to the `strftime` function for formatting.

Additionally, the `DateTime` module also has functions for parsing and manipulating dates and times, making it a useful tool for any date-related operations in your Elixir programs.

## See Also

- [Elixir DateTime module](https://hexdocs.pm/elixir/DateTime.html)
- [Erlang strftime format](http://erlang.org/doc/man/os.html#date-1)