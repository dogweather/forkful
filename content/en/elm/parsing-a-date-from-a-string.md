---
title:                "Parsing a date from a string"
html_title:           "Elm recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string means converting a date in its textual form (such as "May 20th, 2021") into a computer-readable format. This is commonly done by programmers to manipulate, compare, and format dates in their applications.

## How to:

To parse a date from a string in Elm, we can use the `Date.fromString` function. This function takes in a string as its argument and returns a `Maybe Date` value, which means it will either return `Nothing` if the string cannot be parsed into a date, or `Just date` if it is successful.

```
Elm Date.fromString "May 20th, 2021"
```
Output: `Just (Date.fromDate 2021 May 20 0 0 0 0)`

To handle different date formats, we can use the `Date.fromStringWith` function, which takes in a list of formats to try and returns the first successfully parsed date.

```
Elm Date.fromStringWith ["dd/MM/yyyy", "yyyy-MM-dd"] "20/05/2021"
```
Output: `Just (Date.fromDate 2021 May 20 0 0 0 0)`

We can also specify a specific time zone by using the `Date.fromStringInTimezone` function and passing in the desired time zone as the last argument.

```
Elm Date.fromStringInTimezone "May 20th, 2021" "America/New_York"
```
Output: `Just (Date.fromDate 2021 May 20 0 0 0 0)`

## Deep Dive:

In the past, parsing dates was a tedious and error-prone task for programmers, as different programming languages and systems had their own ways of representing dates and times. The introduction of standardized date and time formats, such as ISO 8601, has made this process much easier and more uniform across different platforms.

While Elm provides a robust date parsing module, alternative libraries such as `elm-tools/date-extra` also offer additional capabilities and flexibility in handling date parsing and manipulation.

The `Date.fromString` function in Elm internally uses the `elm/parser` library, which allows for customizable and efficient parsing of various textual data, not just dates.

## See Also:

- Elm Date module documentation: https://package.elm-lang.org/packages/elm/core/latest/Date
- elm-tools/date-extra library: https://package.elm-lang.org/packages/elm-tools/date-extra/latest/
- elm/parser library: https://package.elm-lang.org/packages/elm/parser/latest/