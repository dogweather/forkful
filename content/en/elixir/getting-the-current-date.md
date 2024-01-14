---
title:    "Elixir recipe: Getting the current date"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to get the current date in your Elixir program? Maybe you want to use it for time-sensitive operations or simply display it to the user. Whatever the reason, in this blog post we will explore how to get the current date in Elixir.

## How To

To get the current date in Elixir, we can use the `Date.utc_today()` function from the `Date` module. Let's take a look at an example of how we can use it in our code:

```Elixir
# Import the Date module
import Date

# Get the current date in UTC format
current_date = Date.utc_today()
```

This will return a `Date` struct which contains the current date information. We can then access different components of the date, such as the day, month, or year. For example, we can get the current day of the year by using `current_date.day`, or the current year by using `current_date.year`.

We can also format the date in any way we want by using the `Date.format/2` function. This function takes in a format string as the first parameter, and the date struct as the second parameter. Let's see an example of formatting the current date as a string in the format of `YYYY-MM-DD`:

```Elixir
# Format the current date as a string
formatted_date = Date.format("YYYY-MM-DD", current_date)

# Outputs "2019-09-23"
IO.puts(formatted_date)
```

## Deep Dive

Behind the scenes, the `Date utc_today()` function uses the Erlang `:calendar.universal_time/0` function to get the current date. This function returns the number of seconds since January 1, 1970, commonly known as Unix timestamp. The `Date` module then uses this timestamp to calculate the current date.

One important thing to note is that the `Date` module only works with UTC time. If you need to work with local time, you will need to use the `NaiveDateTime` module and convert it to the correct timezone.

## See Also

- [Elixir Date module documentation](https://hexdocs.pm/elixir/Date.html)
- [Erlang calendar module documentation](http://erlang.org/doc/man/calendar.html)
- [Elixir NaiveDateTime module documentation](https://hexdocs.pm/elixir/NaiveDateTime.html)

Now you know how to get the current date in Elixir! So go ahead and use this knowledge in your next Elixir project.