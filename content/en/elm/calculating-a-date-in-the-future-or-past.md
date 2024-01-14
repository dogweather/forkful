---
title:                "Elm recipe: Calculating a date in the future or past"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is an essential task in any programming language. It allows for scheduling events, implementing reminder systems, and many other practical applications.

## How To

To calculate a date in the future or past, we can use the `Time` library in Elm. First, we need to define the reference date from which we want to calculate. We can use the `now` function to get the current date and time.

```Elm
now : Task x Time
```

We can then use `Unix.toTime` to convert a Unix timestamp to a `Time` value. For example, to get the time for tomorrow, we can use the following code:

```Elm
tomorrow : Time
tomorrow =
    now
        |> Task.toMaybe
        |> Maybe.andThen (\now -> now.timestamp |> Unix.toTime)
        |> Maybe.withDefault now
        |> Time.tomorrow
```

This code first gets the current time using `now`, converts it to a `Maybe` value using `Task.toMaybe`, and then extracts the timestamp using `Maybe.andThen`. If the value is `Nothing`, we use `now` as a default value. Finally, we use the `tomorrow` function from the `Time` library to get the time for tomorrow.

We can also calculate a date in the past by subtracting a certain number of milliseconds from the reference date. For example, to get the time for 7 days ago, we can use the following code:

```Elm
sevenDaysAgo : Time
sevenDaysAgo =
    now
        |> Task.toMaybe
        |> Maybe.andThen (\now -> now.timestamp |> Unix.toTime)
        |> Maybe.withDefault now
        |> Time.subtract (Time.millisecond * 7 * 24 * 60 * 60 * 1000)
```

Here, we are converting the timestamp to a `Time` value and then subtracting 7 days (in milliseconds) from it.

## Deep Dive

Calculating dates in the future or past can be a bit tricky, especially when dealing with time zones and daylight saving time. It is essential to understand how the `Time` library handles time to ensure accurate results.

In Elm, time is represented as a `Posix` value, which is the number of milliseconds since `January 1, 1970, 00:00:00` UTC. This means that time is always based on UTC; however, the `Time` library provides functions to convert to and from local time.

When calculating dates, it is essential to take into account the time zone and daylight saving time for the desired date. We can use the `Convert` library to convert a `Time` value to a specific time zone. This can be helpful when working with international clients or events.

## See Also

- [Elm Time library documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Convert library documentation](https://package.elm-lang.org/packages/justinmimbs/elm-time/latest/)
- [Blog post on calculating dates with Elm](https://dev.to/hwaxxer/calculating-dates-in-elm-2ak)