---
title:    "Elm recipe: Getting the current date"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to display the current date in your Elm program? Whether it's for a simple reminder or a more complex feature, getting the current date is an essential task for many programs.

## How To
Getting the current date in Elm is actually quite straightforward. We'll be using the `Time` library, which provides functions for working with date and time. Let's take a look at a simple example:

```Elm
import Time exposing (today, Date)

-- get today's date
currentDate : Date
currentDate = today
```

In this example, we import the `today` and `Date` functions from the `Time` library. We can then use the `today` function to get the current date, which is a `Date` type. We can then store it in a variable called `currentDate` for future use.

But what if we want to display the current date in a specific format, such as "Monday, October 04, 2021"? We can use the `format` function from the `Date` module to achieve this. Here's an example:

```Elm
import Time exposing (today, Date)
import Date exposing (format)

-- get today's date and format it
currentDate : String
currentDate = today
    |> format "%A, %B %d, %Y"
```

In this example, we use the `format` function to specify the format we want our date to be in. We use `%A` to get the full weekday name, `%B` for the full month name, `%d` for the day of the month, and `%Y` for the full year. The result will be stored in the `currentDate` variable.

## Deep Dive
Behind the scenes, the `Date` type in Elm is actually a representation of a date in the ISO-8601 format (e.g. "2021-10-04"). This makes working with dates in Elm much easier, as there are no concerns about different date formats or time zones.

The `Time` library also provides functions for comparing dates, adding or subtracting time, and converting between time zones. This can be incredibly useful for more complex programs that need to handle different time zones or perform calculations with dates.

## See Also
- [Elm Date module documentation](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm Time module documentation](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Elm programming language website](https://elm-lang.org/)