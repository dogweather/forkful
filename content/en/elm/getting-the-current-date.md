---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to display the current date in your web application? Look no further, because Elm has got you covered! In just a few simple steps, you can easily get the current date and use it to dynamically update your UI.

## How To
Getting the current date in Elm is actually quite simple. First, we need to import the `Date` module.

```Elm
import Date exposing (Date)
```

Next, we can use the `now` function from the `Date` module to get the current date and time. This function returns a `Result` type, which means it can either be successful or it can fail. To handle this, we can use the `case` expression.

```Elm
case Date.now of
    Ok dateTime ->
        -- do something with the current date and time
    Err error ->
        -- handle error
```

Once we have the `dateTime` value, we can use the `Date.toString` function to format it in whichever way we want. For example, if we want to display the date in the format of "Month Day, Year", we can use the following `case` expression.

```Elm
case Date.now of
    Ok dateTime ->
        Date.toString "%B %d, %Y" dateTime
        -- this will give us something like "April 25, 2021"
    Err error ->
        -- handle error
```

## Deep Dive
When using the `now` function, there are a few things to keep in mind. Firstly, the current date and time returned is based on the user's system clock. This means that if the user has their system clock set to a different date or time, it will affect the value returned by the `now` function.

Also, the `Date` module provides many other functions that can be useful when working with dates and times. For example, you can use the `fromCalendarDate` function to create a `Date` value from a specific date, or the `fromInt` function to create a `Date` value from a Unix timestamp. Be sure to check out the official Elm documentation for more information on these functions.

## See Also
- Official Elm Documentation on the Date module: https://package.elm-lang.org/packages/elm/time/latest/Date
- How to Format Dates in Elm: https://www.youtube.com/watch?v=wK52UvkJbIY
- Creating a Countdown Timer in Elm with Dates and Times: https://dev.to/johnrkeogh/creating-a-countdown-timer-in-elm-using-titles-1p99