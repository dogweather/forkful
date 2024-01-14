---
title:    "Elm recipe: Calculating a date in the future or past"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past is a common task in many programming applications. For example, you may want to schedule a future event or calculate the next payment date for a recurring expense. In this blog post, we will explore how to do this in the Elm programming language.

## How To
To calculate a date in the future or past, we will use the `Time` module in Elm. First, we need to import this module into our code:

```Elm
import Time exposing (..)
```

Next, we can use the `add` function to add a certain number of units (such as days, months, or years) to a given time value. Here is an example of calculating the date 7 days from now:

```Elm
sevenDaysLater = Time.add Days 7 currentTime
```

The `currentTime` value represents the current time and date. The `add` function will return a `Posix` value, which represents the date and time in seconds since January 1, 1970.

We can also subtract units from a given time value using the `sub` function. For example, here is how we would calculate the date 1 year ago:

```Elm
oneYearAgo = Time.sub Years 1 currentTime
```

Both the `add` and `sub` functions can take a variety of units including seconds, minutes, hours, days, weeks, months, and years.

## Deep Dive
Under the hood, the `Time` module in Elm utilizes the `Posix` type to represent time values. This type is based on the Unix timestamp format and is used to accurately represent dates and times regardless of timezone differences.

It is important to note that when using the `add` and `sub` functions, we are working with `Posix` values and not traditional date and time objects. Therefore, we may need to convert these values to a more readable format using the `Date` module if needed.

## See Also
- Elm Time Module Documentation: https://package.elm-lang.org/packages/elm/time/latest/Time
- Unix Timestamp Converter: https://www.unixtimestamp.com/