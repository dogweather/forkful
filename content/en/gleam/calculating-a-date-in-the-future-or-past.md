---
title:                "Gleam recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why 

Have you ever needed to calculate a date in the past or future in your programming projects? Maybe you're building a reservation system or scheduling tool. Whatever the reason may be, Gleam has got you covered with its built-in date manipulation functions. In this blog post, we'll explore how to use Gleam to easily calculate future or past dates.

## How To 

To calculate a date in the future or past, we'll be using the `Date` module from the standard library. Let's start by importing it at the top of our Gleam file:

```Gleam
import gleam/stdlib/Date
```

Next, let's define a function that takes in an integer representing the number of days and returns a future date based on that number. We'll call this function `add_days`:

```Gleam
pub fn add_days(days: Int) -> Date {
  Date.add_days(Date.from_gregorian(2021, 07, 01), days)
}
```

In this code block, we first create a date using the `from_gregorian` function, passing in the year, month, and day in that order. Then, we use the `add_days` function to add the specified number of days to the given date. Running this function with a value of 10 for `days` would return the date July 11, 2021.

Similarly, we can also calculate a date in the past by using negative values for `days`:

```Gleam
pub fn subtract_days(days: Int) -> Date {
  Date.subtract_days(Date.from_gregorian(2021, 07, 01), days)
}
```

This function would subtract the specified number of days from the given date, returning June 21, 2021 if `days` is 10.

## Deep Dive 

Now, let's dive deeper into how Gleam handles dates. Gleam follows the Gregorian calendar system and represents dates as tuples with three elements: year, month, and day. This makes it easy to manipulate dates by simply changing the values of each element.

Additionally, Gleam also has functions for getting the current date, getting the number of days in a specific month, comparing dates, and more. Learn more about these functions and their usage in the Gleam documentation.

## See Also 
- [Gleam Date Module Documentation](https://gleam.run/modules/gleam_stdlib/Date.html)
- [Gleam Standard Library Documentation](https://gleam.run/modules/gleam_stdlib.html)
- [Gregorian Calendar System](https://en.wikipedia.org/wiki/Gregorian_calendar)