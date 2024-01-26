---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:36:52.638876-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string means transforming text into a date data type. Programmers often need to convert user input or text file contents into structured dates for processing and manipulation.

## How to:

Haskell offers multiple ways to parse dates, but let's focus on the `time` library and a simple example using `parseTimeM`. Ensure you have the `time` package installed.

```haskell
import Data.Time.Format (parseTimeM, defaultTimeLocale)

main :: IO ()
main = do
  let dateString = "2023-03-21"
  let parsedDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString :: IO (Maybe Day)
  
  result <- parsedDate
  case result of
    Just day -> putStrLn $ "Parsed date: " ++ show day
    Nothing -> putStrLn "Failed to parse date."

-- Output should be: Parsed date: 2023-03-21
```

## Deep Dive

Historically, date parsing has been handled differently across languages and libraries, with many using variations on the `strftime` patterns from C. Haskell's `time` library mirrors this approach for consistency. Alternatives to `time` include using the `old-time` package, which is now deprecated, or third-party libraries like `thyme` or `chronos`.

Implementation-wise, parsing in Haskell is type-safe, hence the use of `Maybe` in the example to handle parsing failures. The `parseTimeM` function utilizes type inference to determine the return type, making it flexible. Understanding the format specifiers, like `%Y-%m-%d` for year-month-day, is crucial.

Haskell's strong type system ensures that once a date is parsed, it is clear and unmistakable what type it is, reducing runtime errors related to date manipulation. However, this strictness means you must handle cases when the input does not match the expected pattern, hence the pattern-matching on `Just` and `Nothing`.

## See Also

- Haskell `time` library documentation: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time)
- "Learn You a Haskell" guide on dates and times: [http://learnyouahaskell.com/](http://learnyouahaskell.com/) - (search for the "Data.Time" section)
- Format specifiers for `Data.Time.Format`: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime)
