---
title:                "Converting a date into a string"
html_title:           "Elixir recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string means taking a date object and turning it into a textual representation, such as "January 1st, 2022". Programmers often do this in order to display dates in a user-friendly and readable format.

## How to:

Converting a date into a string in Elixir is a simple task. First, we need to import the `Calendar` module which contains functions for working with dates. Then, we can use the `to_string` function to convert a date into a string. Let's see an example:

```Elixir
import Calendar

date = ~D[2022-01-01]
date_string = to_string(date)

IO.puts(date_string)
# Output: "January 1st, 2022"
```

We can also specify a format for the string by using the `Calendar.strftime` function. This allows us to customize the output according to our needs. For example:

```Elixir
import Calendar

date = ~D[2022-01-01]
date_string = Calendar.strftime(date, "%A, %B %d, %Y")

IO.puts(date_string)
# Output: "Saturday, January 01, 2022"
```

## Deep Dive

Converting a date into a string has been a common practice in programming since the early days. It allows us to present dates to users in a way that makes sense to them, regardless of their location or language. Besides Elixir's built-in `Calendar` module, there are also third-party libraries available, such as `Timex`, which provide more advanced features and functionalities for working with dates and time.

When converting a date into a string, we need to consider different date formats and conventions around the world. Elixir's `Calendar` module takes these into account and allows us to customize the output accordingly. Additionally, the `Calendar` module also supports time zones and daylight saving time, making it a reliable choice for working with dates and time.

## See Also
- [Documentation on Date and Time in Elixir](https://hexdocs.pm/elixir/Date.html)
- [Timex library for working with dates and time in Elixir](https://hexdocs.pm/timex/Timex.html)
- [Format string options for converting dates into strings using `Calendar.strftime`](https://hexdocs.pm/timex/Timex.html#strftime/3)