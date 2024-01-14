---
title:    "Elm recipe: Converting a date into a string"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

Converting dates into strings may seem like a simple task, but it can become a bit trickier when working with different formats and time zones. However, using Elm's built-in functions, we can easily convert a date into a string without any hassle.

## How To

To convert a date into a string in Elm, we can use the `toString` function. This function takes in a `Date` value and returns a string representation of the date.

Here is an example of how we can use this function:

```Elm
import Date exposing (Date)

dateToString : Date -> String
dateToString date =
    Date.toString "MM/dd/yyyy" date

-- Output: "07/22/2021"
```

In the example above, we first import the `Date` module which provides us with the necessary functions to work with dates. Then, we define a function `dateToString` which takes in a `Date` value and returns a string representation of the date in the format of "MM/dd/yyyy". We can then call this function with a `Date` value and it will return the date as a string.

## Deep Dive

The `toString` function is actually a combination of two functions, `format` and `fromDate`. The `format` function takes in a format string and a `Date` value and returns a string representation of the date in the specified format. The `fromDate` function takes in a `Date` value and returns a list of integers representing the year, month, and day of the date.

Using these two functions, we can create more customized date-to-string conversions. For example, instead of using a predefined format, we can use `format` to specify our own formatting rules. Or, we can use `fromDate` to extract specific information from a date and use it in our string.

## See Also

- [Official Elm Date documentation](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Converting a string to a date in Elm](https://www.rivermanmedia.com/convert-a-string-to-a-date-in-elm/) (blog post by Riverman Media)