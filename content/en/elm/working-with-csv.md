---
title:                "Working with csv"
html_title:           "Elm recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Dealing with CSV or Comma-Separated Values is a common task for programmers. CSV is a popular file format used to store tabular data, similar to spreadsheets. It consists of rows and columns, with each row representing a record and each column representing a field. Programmers usually work with CSV files to read or write data to and from a database or to export data from an application for further analysis.

## How to:

Working with CSV in Elm is straightforward and efficient. The `elm-csv` package provides useful functions for parsing and generating CSV files. Let's take a look at an example of how to read data from a CSV file and display it on the browser.

```Elm
import Csv exposing (parse)

fileData : String
fileData = 
  "Name,Age,Gender\nJohn,30,Male\nMaria,25,Female"

{ data, fields } = 
  case parse fileData of
    Ok result ->
      result
    Err _ ->
      ([], [])

table : List (List String) -> Html msg
table rows =
  table [] (List.map row rows)

row : List String -> Html msg
row data =
  tr [] (List.map (cell "") data)

cell : List (Attribute msg) -> String -> Html msg
cell att content =
  td att [ text content ]

main =
  table data
```

In this example, we import the `parse` function from the `Csv` package which takes a string of CSV data and returns a `Result` with a tuple containing the parsed data and the headers. We then use pattern matching to extract these values and display them in a table using Elm's `Html` module.

## Deep Dive

CSV has been around since the 1970s and has become a popular format due to its simplicity and compatibility with most systems. It is also highly configurable, allowing for different delimiters, line terminators, and encodings.

Besides using the `elm-csv` package, another way of working with CSV in Elm is by using the `Url` library to make HTTP requests to CSV files. This method can be useful when working with larger CSV files as it allows for streaming data instead of loading the entire file into memory.

Internally, `elm-csv` uses a parser combinator library called `nom` to parse the CSV data efficiently. This library is also used in other languages such as Rust and Python, making it a familiar tool for developers.

## See Also

- [elm-csv package](https://package.elm-lang.org/packages/elm-community/csv/latest/)
- [Url library](https://package.elm-lang.org/packages/elm/http/latest/)
- [nom library](https://github.com/Geal/nom)