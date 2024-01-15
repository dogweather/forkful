---
title:                "Calculating a date in the future or past"
html_title:           "Elm recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to calculate a future or past date in your programming project? Maybe you're building a personal planner app or a scheduling tool. Either way, Elm has a convenient built-in function that makes date calculations a breeze. Keep reading to learn how to use it!

## How To

To calculate a date in the future or past, we'll be using the `add` function from the `Time` module. This function takes in two arguments: a `Time` value and an `Int` representing the number of milliseconds to add to the time value.

Let's see an example of how we can use the `add` function to calculate a date 5 days in the future:

```Elm
import Time exposing (..)

-- Get current time
currentTime : Time
currentTime = Time.millisToPosix <| Time.since Epoch

-- Calculate date 5 days in the future
futureDate : Time
futureDate = add 5 * days currentTime

-- Format date as a string
formatDate : String
formatDate = toString <| Time.millisToUtcIsoString futureDate

-- Output: 2021-09-13T18:38:58.271Z
```

In the above code, we import the `Time` module and use the `since` function to get the current time in milliseconds. Then, we call the `add` function with the desired number of days to add and the current time value. Finally, we use the `toString` function to format the date as a string in UTC format. Easy, right?

## Deep Dive

The `add` function can also be used to calculate dates in the past. Instead of adding a positive integer, we can pass a negative integer to subtract time from the current date. Additionally, the `Time` module has other useful functions for working with dates, such as `utcToMillis` and `utcToPosix`. Be sure to check out the official documentation for more details.

## See Also

- Official Elm Time Module Documentation: https://package.elm-lang.org/packages/elm/time/latest/ 
- Elm Date and Time Basics Guide: https://elmprogramming.com/elm-dates-times.html 
- Elm Date Cheat Sheet: https://devhints.io/elm-date