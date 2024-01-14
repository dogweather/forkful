---
title:    "Elixir recipe: Converting a date into a string"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

### Why

If you're new to Elixir programming, you may be wondering why converting a date into a string is an important skill to have. However, as you start building more complex applications, you'll likely encounter situations where you need to manipulate dates and display them in a specific format. Converting a date into a string allows you to customize the way dates are presented and make your applications more user-friendly.

### How To

Converting a date into a string in Elixir is a fairly straightforward process. Let's look at an example using the `DateTime` module from the `Ecto` library:

```Elixir
# Import the DateTime module
import DateTime

# Set a date
date = ~D[2021-01-01]

# Convert the date into a string using DateTime.to_string/1
date_string = DateTime.to_string(date)

# Output: "2021-01-01 00:00:00"
IO.puts(date_string)
```

In this example, we first import the `DateTime` module and then set a date using the `~D` sigil. Next, we use the `DateTime.to_string/1` function to convert the date into a string. One thing to note is that the output of `DateTime.to_string/1` will include the time as well, even if the original date did not have a time component.

If you want to customize the format of the string, you can use the `to_string!/2` function and specify a format as the second argument. For example:

```Elixir
# Output: "January 01, 2021"
date_string = DateTime.to_string!(date, "{Month} {0} {YYYY}")
```

The format string uses specifiers to define how the date should be formatted. For a full list of specifiers, you can refer to the official Elixir documentation for the `DateTime` module.

### Deep Dive

Under the hood, `DateTime.to_string/1` uses the `:calendar.format_time/2` function to convert the date into a string. This function uses the `:calendar.local_time_zone/0` to determine the timezone of the date and adjust it accordingly. If you need to convert a date into a string without adjusting for timezones, you can use `:calendar.universal_time/1` instead.

Additionally, Elixir also provides the `NaiveDateTime` module which handles dates and times without considering the timezone. This can be useful for cases where you want to perform date calculations without worrying about timezones.

### See Also

If you want to dive deeper into date and time manipulation in Elixir, check out these resources:

- [Elixir DateTime module](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir NaiveDateTime module](https://hexdocs.pm/elixir/NaiveDateTime.html)
- [Elixir Calendar module](https://hexdocs.pm/elixir/Calendar.html)