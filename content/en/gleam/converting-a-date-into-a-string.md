---
title:    "Gleam recipe: Converting a date into a string"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Converting dates into strings is a common task in programming, especially when working with time-sensitive data or user-facing interfaces. By converting dates into strings, we can easily display them in a readable format and manipulate them for different purposes.

## How To

In Gleam, converting a date into a string can be done using the `format` function from the `gleam-time` module. Let's take a look at an example:

```Gleam
import gleam/time

let my_date = time.now()
let date_string = time.format("%Y-%m-%d", my_date)
```

In this code, we first import the `gleam/time` module to have access to the `format` function. Then, we create a new variable `my_date` using the `now` function, which returns the current date and time. Finally, we use the `format` function to convert the date into a string, using the format string `"%Y-%m-%d"` which specifies that the date should be displayed in the format of year-month-day. 

The output of this code would be a string like `2021-12-31`, depending on the current date. We can also specify different formats, such as `"%d/%m/%Y"` for a day-month-year format or `"%A, %B %d, %Y"` for a more readable format like "Monday, January 1st, 2021".

## Deep Dive

The `format` function takes in two arguments - the format string and the date to be formatted. The format string is a set of characters that define the format in which the date should be displayed. These characters have different meanings, such as `%Y` for the full year, `%m` for the month in two digits, `%d` for the day in two digits, and so on. 

We can also include other characters, like `-` or `:` to add separators between the different parts of the date. Additionally, we can use special characters like `%B` for the full month name or `%a` for the abbreviated weekday name.

It's important to note that the order in which we include the characters in the format string will determine the format of the date. For example, `%Y-%m-%d` will display the year first, followed by the month and day, while `%m-%d-%Y` will display the month first, followed by the day and year.

## See Also

- [Gleam API documentation on `gleam-time`](https://gleam.run/documentation/action/gleam-time)
- [ISO 8601 standard for date and time representation](https://en.wikipedia.org/wiki/ISO_8601)