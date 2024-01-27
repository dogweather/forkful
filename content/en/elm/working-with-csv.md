---
title:                "Working with CSV"
date:                  2024-01-19
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) means reading and writing data in a text format where each line has values split by commas. Programmers use CSV because it's a simple format supported by many tools and systems, making it great for data interchange.

## How to:

Elm doesn't have a built-in CSV parser, but you can easily add one with a package like `elm-csv`. Here's a quick example of parsing CSV data:

```Elm
import Csv

csvData : String
csvData =
    "name,age\nAlice,30\nBob,25"

parseCsv : String -> Result Csv.Error (List (List String))
parseCsv data =
    Csv.decode data

main =
    case parseCsv csvData of
        Ok rows ->
            -- do something with the rows
            text (String.join "," (List.head rows |> Maybe.withDefault []))
            
        Err error ->
            -- handle the error
            text (Csv.Error.toString error)
```

Sample output for the successful case, displaying the headers:

```
name,age
```

## Deep Dive

CSV has been around since the early 1970s; it's so simple that it predates actual standards. Alternatives include JSON and XML, but CSV is still preferred when dealing with tabular data that's heavy on numbers and short on structure. In Elm, since it's a front-end language, you'll work by either receiving CSV from a backend or processing a local file uploaded by the user. Implementing this requires knowledge of Elm's ports for JS interop or file package for uploads.

## See Also

- Elm guide on interop with JavaScript: [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
