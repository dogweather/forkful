---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of taking a text input and converting it into a machine-readable format, usually a Date object. Programmers do this because text is often the most common and universal format for data, especially when pulling information from APIs, forms, or databases.

## How To:

Let's use Elm's `Date.fromString` function. Given a date input as a string, this function will return a `Result Result.DateParseError Date` type. If parsing succeeds, a `Date` object is returned, otherwise, `Result.DateParseError` object containing a description of the error.

Here's a simple example of parsing a date from a string:

```Elm
import Date exposing (Date)
import Date.Extra.Formats as Formats exposing (formatsDate)

parseStringtoDate : String -> Result String Date
parseStringtoDate dateString =
    formatsDate ["YYYY-MM-DD", "YYYY/MM/DD", "DD-MM-YYYY", "DD/MM/YYYY"] dateString

-- Let's test it
parseStringtoDate "2021-12-31"
-- -> Ok <| Date 2021 12 31 0 0 0 0
```

## Deep Dive

The `Date.fromString` function comes from Elm's core `Date` module, which dates back to the first public version of Elm. It has served as a simple and straight-forward method for parsing dates since its inception.

An alternative method of parsing dates would be to utilize Regex implementations. However, Elm is a language favouring simplicity and explicitness over abbreviation. Therefore, using `Date.fromString` is the preferred way, unless you have a specific use case that this function does not cover.

The parsing mechanism works by looking for patterns in the string provided, such as 'YYYY', 'MM', or 'DD', and uses these to break down and understand the composition of the date. If it fails to find an expected pattern, it returns an error highlighting where the parsing failed.

## See Also

1. [Official Elm Date Documentation](https://package.elm-lang.org/packages/elm/time/latest/)

2. [Elm's Date.Extra module](https://package.elm-lang.org/packages/elm-community/date-extra/latest/)

3. [Discussion on Date Parsing in Elm](https://discourse.elm-lang.org/t/parsing-date-strings-in-elm-0-19/2671)