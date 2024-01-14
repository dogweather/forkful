---
title:    "Elm recipe: Converting a date into a string"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Understanding how to convert a date into a string is an essential skill for any Elm programmer. Converting a date into a string allows you to display dates in a user-friendly format, making your application more user-friendly and intuitive.

## How To

In Elm, dates are stored as a type called `Posix`, which represents the number of milliseconds since January 1, 1970, at midnight UTC. To convert a `Posix` value into a human-readable date string, we will use the `Time` library. Let's look at an example of converting a `Posix` value into a string representing the current date:

```elm
import Time exposing (posixToMillis, millisToPosix, toString, Date)

-- Get current date as `Posix` value
currentTime : Time.Posix
currentTime =
  Time.now

-- Convert `Posix` value to `Date` type
currentDate : Time.Date
currentDate =
  Time.millisToPosix currentTime

-- Convert `Date` type to string
dateString : String
dateString =
  Time.toString Date.utc "%Y-%m-%d" currentDate
```

In this example, we first import the `Time` library which has functions for working with dates and times. We then use the `now` function to get the current time as a `Posix` value. Next, we use the `millisToPosix` function to convert this value into a `Date` type. Finally, we use the `toString` function to format the `Date` into a string, using the `"%Y-%m-%d"` format for a date in the UTC timezone.

Running this code will give us an output of "2021-01-25" as the current date in the UTC timezone. You can play around with different formats and timezones to get your desired output.

## Deep Dive

As mentioned earlier, Elm stores dates as `Posix` values, which is a common way of representing dates in programming. This format allows for easy comparisons and calculations with dates. However, for displaying dates in a user-friendly format, we must convert them into a string.

The `Time` library provides many functions for working with dates and times, including formatting and parsing strings. It also has functions for performing operations on dates, such as adding and subtracting days, hours, and other time units.

When converting a date into a string, it's essential to consider the timezone to get accurate results. The `Date` type in Elm allows for specifying the timezone when converting to a `String` to ensure the correct date is displayed.

## See Also

- Elm Time Library Documentation: https://package.elm-lang.org/packages/elm/time/latest/
- Date and Time Formatting in Elm: https://guide.elm-lang.org/effects/time.html#date-time-formatting
- Elm DateTime Cheat Sheet: https://korban.net/elm/elm-datetime-cheat-sheet/