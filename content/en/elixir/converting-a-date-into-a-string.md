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

## Why 

Converting a date into a string is a commonly used operation in many programming languages, including Elixir. It allows for easier manipulation and display of date information. In this article, we will explore the different ways to convert a date into a string using Elixir.

## How To 

To convert a date into a string in Elixir, we can use the `~D` sigil or the `to_string/2` function.

```
# Using the ~D sigil

iex> ~D[2021-09-05]
"2021-09-05"

# Using the to_string/2 function

iex> Date.to_string(~D[2021-09-05])
"2021-09-05"
```

Both methods produce the same output, a string representation of the date. However, the `to_string/2` function provides more flexibility as it allows for additional formatting options.

```
# Using the to_string/2 function with formatting options

iex> Date.to_string(~D[2021-09-05], "yyyy/MM/dd")
"2021/09/05"

iex> Date.to_string(~D[2021-09-05], "MMM dd, yyyy")
"Sep 05, 2021"
```

We can also convert dates to strings with time information using the `~DT` sigil or the `to_string/3` function.

```
# Using the ~DT sigil

iex> ~DT[2021-09-05 15:30:00]
"2021-09-05 15:30:00Z"

# Using the to_string/3 function

iex> DateTime.to_string(~DT[2021-09-05 15:30:00], "MMM dd, yyyy - HH:mm:ss")
"Sep 05, 2021 - 15:30:00"
```

Similarly, we can apply different formatting options to the date and time string.

## Deep Dive 

Dates in Elixir are represented by the `Date` struct, while date and time together are represented by the `DateTime` struct. These structs have built-in functions such as `to_string/2` and `to_string/3` to convert them into strings. Additionally, the `Calendar` module provides more functions for formatting dates and times, including `to_string!/2` and `to_string!/3` which raise an error if the provided format is invalid.

It's worth noting that converting a date into a string and then parsing it back into a date may not always result in the same original date due to different formatting options and precision. It is recommended to stick to one consistent date format and avoid converting dates back and forth unnecessarily.

## See Also 

- [Elixir DateTime Module](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Calendar Module](https://hexdocs.pm/elixir/Calendar.html)
- [Date and Time Conversion in Elixir](https://elixir-lang.org/getting-started/date-and-time.html#conversion)