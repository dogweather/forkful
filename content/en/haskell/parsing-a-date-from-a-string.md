---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# String Date Parsing with Haskell

## What & Why?
Parsing a date from a string is the action of reading a date format from a string data type. Programmers do this to transform date information stored in strings into useable date objects for computations and manipulations.

## How to:
We'll use the `parseTimeM` function from `Data.Time.Format` module in Haskell, along with functions `defaultTimeLocale`, `True`, and `%Y-%m-%d` to specify the desired date format.

```Haskell
import Data.Time
import Data.Time.Format

-- A simple date string
dateStr = "2022-10-15" 

-- Call to parseTimeM function
maybeDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr :: Maybe UTCTime
```

The **result** `Maybe UTCTime` instance might be `Just <UTCTime value>` if parsing succeeds and `Nothing` if it fails.

Now let's **print** our date.

```Haskell
case maybeDate of
    Just date -> print date
    Nothing -> putStrLn "Oops, invalid date format."
```

## Deep Dive
Historically, date parsing was more manual, with coders specifying each part of the date. Now, libraries like `Data.Time.Format` offer simplified methods. 

Alternative libraries exist, like `time-parsers` and `formatted-time`, each with their distinct advantages. 

The `parseTimeM` functions are locale-aware that parses time textual representation, returning a `Maybe Time` value according to a format specification, where `Nothing` indicates a parsing failure. It allows programmers to handle date parsing errors more explicitly.

## See Also
For more details and date formatting options, check the following links:

- [`Data.Time.Format` module documentation](https://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time-Format.html)
- [Haskell date and time utility libraries](https://haskell.libhunt.com/categories/310-date)