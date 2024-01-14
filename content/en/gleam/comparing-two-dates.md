---
title:                "Gleam recipe: Comparing two dates"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing dates is a common task in programming, especially when working with data that involves time and date information. In Gleam, there are built-in functions that make it easy to compare two dates and determine the relationship between them. In this blog post, we will explore the different ways you can compare dates in Gleam and how it can be useful in your projects.

## How To

To compare two dates in Gleam, we will use the `Date` module. This module provides various functions for creating, parsing, and comparing dates. Here's an example of how we can use the `Date` module to compare two dates:

```Gleam
import Date

let january1st = Date.from_ymd(2021, 1, 1)
let december31st = Date.from_ymd(2020, 12, 31)

let result = Date.compare(january1st, december31st)

// Output: GreaterThan
```

In this code, we first import the `Date` module. Then, we create two variables with different dates using the `from_ymd` function and pass in the year, month, and day as arguments. Finally, we use the `compare` function to compare the two dates, which returns an enum with three possible values: `Equal`, `LessThan`, or `GreaterThan`. In this example, the output is `GreaterThan`, indicating that January 1st is a later date than December 31st.

We can also compare specific aspects of dates, such as the year, month, or day, by using the `get` function. Here's an example:

```Gleam
let january2021 = Date.from_ymd(2021, 1, 1)
let january2020 = Date.from_ymd(2020, 1, 1)

let yearComparison = Date.get(january2021, Date.Year) == Date.get(january2020, Date.Year)

// Output: False
```

In this code, we create two variables with dates in different years and use the `get` function to compare the year values. As you can see, the result is `False` since the year values are not equal.

## Deep Dive

Behind the scenes, Gleam uses the `Calendar` module for comparing dates. The `Calendar` module implements the Gregorian calendar, which is the most widely used calendar system worldwide. It takes into account factors such as leap years and time zones to accurately compare dates.

When using the `compare` function, the dates are first converted into their corresponding "day number" and then compared. The "day number" is a number representing the number of days since the start of the year 1 AD. This is a more efficient way of comparing dates compared to comparing each date component individually.

## See Also

- [Gleam Date Module Documentation](https://gleam.run/modules/date.html)
- [Gleam Calendar Module Documentation](https://gleam.run/modules/calendar.html)
- [Gleam Language Documentation](https://gleam.run/documentation)

Happy coding with Gleam!