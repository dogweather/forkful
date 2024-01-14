---
title:                "Elm recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past is a common task in many programming languages, including Elm. It allows for scheduling events, tracking deadlines, or creating calendars.

## How To
To calculate a date in Elm, we can use the `Time` library. First, we need to import the library:

```Elm
import Time
```

To get the current date, we can use the `now` function:

```Elm
let
    current = Time.now
```

To calculate a date in the future or past, we can use the `add` function. It takes in three arguments: the unit of time (seconds, minutes, hours, days, weeks, months, years), the number of units, and the starting date. For example, to calculate a date 3 days from now:

```Elm
let
    future = Time.add Time.days 3 current
```

We can also calculate a date in the past by passing in a negative number. For example, to calculate a date 2 weeks ago:

```Elm
let
    past = Time.add Time.weeks -2 current
```

To format the date to a human-readable format, we can use the `format` function. For example, to display the date in the format "MMM d, yyyy":

```Elm
let
    dateString = Time.format "%b %d, %Y" past
```

The output would be "Jun 11, 2021". It's that simple!

## Deep Dive
Under the hood, Elm uses the `posix` time format, which represents time as the number of milliseconds since January 1st, 1970. This is the standard time format used in many programming languages. The `add` function takes in a duration in milliseconds and adds it to the starting date.

One thing to keep in mind when calculating dates in Elm is that the data type `Time.Posix` is opaque, meaning we cannot access its internal values directly. This is done intentionally to prevent developers from making mistakes when manipulating time.

## See Also
- Elm Time Library [https://package.elm-lang.org/packages/elm/time/latest/](https://package.elm-lang.org/packages/elm/time/latest/)
- Posix Time Documentation [https://lwtech-csd.github.io/documents/posixtime.png](https://lwtech-csd.github.io/documents/posixtime.png)
- Date Calculator [https://datecalculators.com/](https://datecalculators.com/)