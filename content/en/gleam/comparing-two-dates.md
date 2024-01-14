---
title:    "Gleam recipe: Comparing two dates"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Comparing dates is a common task in programming, and using Gleam can make it easier and faster. Whether you need to check if a date is before or after another, or calculate the time difference between two dates, Gleam has got you covered.

## How To

To compare two dates in Gleam, we can use the `DateTime` module. First, we need to import it in our file:

```Gleam
import gleam/datetime
```

Next, we can create two `DateTime` values representing the dates we want to compare. For example:
```Gleam
let date1 = DateTime.new(2021, 9, 1)
let date2 = DateTime.new(2021, 10, 1)
```

To check if `date1` is before `date2`, we can use the `is_before` function and pass in the two `DateTime` values:
```Gleam
DateTime.is_before(date1, date2)
```

This will return `true` if `date1` is indeed before `date2` and `false` otherwise. Similarly, we can use the `is_after` function to check if `date1` is after `date2`.

To calculate the time difference between two dates, we can use the `diff` function. This function takes in two `DateTime` values and returns a `TimeSpan` value representing the time difference between them. For example:
```Gleam
DateTime.diff(date1, date2)
```

This will return the time difference between `date1` and `date2` as a `TimeSpan` value.

## Deep Dive

Under the hood, the `DateTime` module in Gleam uses the `DateTime` data type from the underlying Erlang runtime. This data type allows for precise and efficient date and time calculations. Additionally, Gleam provides functions such as `is_valid` and `is_leap_year` to help with validating and manipulating `DateTime` values.

It's important to note that the `DateTime` values in Gleam are immutable, meaning they can't be changed once created. This ensures the reliability and consistency of date comparisons in your code.

## See Also

For more information on the `DateTime` module and other useful Gleam date and time functions, check out the official Gleam documentation and the GitHub repository for examples and code snippets.

- [Gleam DateTime module documentation](https://gleam.run/modules/gleam_datetime.html)
- [Gleam on GitHub](https://github.com/gleam-lang/gleam)