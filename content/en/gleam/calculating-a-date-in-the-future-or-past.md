---
title:    "Gleam recipe: Calculating a date in the future or past"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past is a crucial aspect of many programming applications. With Gleam, it becomes even easier and more efficient to perform such calculations, resulting in smooth and error-free date processing for your projects.

## How To
To calculate a date in the future or past, we can use the `add_days` function provided by the builtin `Time` module in Gleam. Let's take a look at an example:

```Gleam
// Import the Time module
const Time = import gleam/time

// Calculate a date 3 days from now
let futureDate = Time.add_days(Time.now(), 3)
```

The `add_days` function takes two arguments - the starting date and the number of days to add. It returns a new date object with the specified number of days added. In the above example, we used `Time.now()` to get the current date and time as the starting point.

We can also go back in time by passing a negative number as the second argument. For example:

```Gleam
// Calculate a date 2 days ago
let pastDate = Time.add_days(Time.now(), -2)
```

Additionally, you can pass different time intervals such as hours, minutes, or seconds instead of days, to perform more specific calculations. This gives you greater flexibility in handling date and time data in your projects.

## Deep Dive
Behind the scenes, Gleam uses the `DateTime` struct to represent dates and times. The `add_days` function works by converting the input date into a `DateTime` struct, adding or subtracting the specified number of days, and then converting the result back into a date object.

It's also worth noting that the `Time` module provides various other functions for manipulating dates and times, such as `add_seconds`, `add_hours`, and `add_minutes`, to name a few. Using these functions, you can perform more complex date calculations with ease.

## See Also
- Gleam's official documentation on date and time calculations: [https://gleam.run/documentation/guides/time.html](https://gleam.run/documentation/guides/time.html)
- A tutorial on working with dates and times in Gleam: [https://dev.to/gleamlang/working-with-dates-and-times-in-gleam-1e2e](https://dev.to/gleamlang/working-with-dates-and-times-in-gleam-1e2e)

Happy coding!