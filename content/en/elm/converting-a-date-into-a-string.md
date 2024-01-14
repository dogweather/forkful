---
title:                "Elm recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Why

Converting a date into a string is a common task in web development, particularly when working with user data or creating calendars and schedules. In this blog post, we will explore how to convert dates into strings in the Elm programming language.

# How To

To convert a date into a string in Elm, we can use the `Date.toString` function. This function takes in a `Date` value and returns a `String` that represents the date in ISO 8601 format.

```elm
import Date exposing (Date)

todaysDate : Date
todaysDate = Date.today

toString : String
toString = Date.toString todaysDate

-- Output: "2021-11-04"
```

We can also use `Date.format` to convert a date into a customizable string. This function takes in a `Date` value and a string of formatting codes and returns a `String` with the date formatted according to the codes.

```elm
import Date exposing (Date)

todaysDate : Date
todaysDate = Date.today

format : String
format = Date.format "%d/%m/%Y" todaysDate

-- Output: "04/11/2021"
```

# Deep Dive

Behind the scenes, dates are represented as `Posix` values in Elm. `Date.toString` and `Date.format` function internally use the `Date.toStringPosix` and `Date.formatPosix` functions respectively, passing in the date and formatting codes.

In addition to using the built-in `Date` module, we can also use third-party libraries like elm-time to manipulate and format dates in our code.

# See Also

Check out the official Elm documentation for more information on the `Date` module and its functions:
- https://package.elm-lang.org/packages/elm/core/latest/Date
- https://package.elm-lang.org/packages/ryannhg/date-format/latest/DateFormat