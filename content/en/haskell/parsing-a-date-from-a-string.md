---
title:                "Parsing a date from a string"
html_title:           "Haskell recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of extracting date information from a given string, such as "January 1, 2021". This is commonly done by programmers when working with data that includes date information, such as in databases, spreadsheets, or text files. By parsing dates from strings, programmers can convert the data into a more usable format and perform operations such as sorting and filtering.

## How to:

To parse a date from a string in Haskell, we will use the built-in "time" library. First, we need to import the library at the top of our code:
```Haskell
import Data.Time
```

Next, we will use the `parseTimeM` function to specify the format of our date string and extract the date information. For example, if our date string is "January 1, 2021", we can use the following code to extract the month and year:
```Haskell
dateString = "January 1, 2021"
dateInfo <- parseTimeM True defaultTimeLocale "%B %d, %Y" dateString :: Maybe Day
```

The `parseTimeM` function takes four parameters: a `Bool` indicating if the time should be validated, the desired time format using the `%` symbols, the string to be parsed, and the expected output type. In this case, we are using `Maybe Day` as our output type since the function may not be able to parse the string correctly.

We can then use pattern matching to handle the `Maybe` result and extract the date information:
```Haskell
case dateInfo of
  Just date -> putStrLn $ "Month: " ++ show (toGregorian date)
  Nothing -> putStrLn "Unable to parse date"
```

This will output:
```
Month: (January,1,2021)
``` 
which is the month, day, and year in tuple format. 

## Deep Dive:

The `time` library was introduced in 2003 and was inspired by the similar `C` library. It allows for formatting and parsing of date and time data in a simple and efficient manner. 

An alternative to using the `time` library in Haskell is the `date` library, which is more lightweight and has simpler functions for parsing dates. However, it offers less control over the formats and may not support all required formats.

The `parseTimeM` function works by using the `%` symbols to specify the format of the date string. For example, `%B` represents the full month name and `%Y` represents the four-digit year. The symbols are based on the `strptime` function in the `C` language.

## See Also:

- [Haskell time library documentation](https://hackage.haskell.org/package/time)
- [Haskell date library documentation](https://hackage.haskell.org/package/date)
- [GNU C Library time input format](https://www.gnu.org/software/libc/manual/html_node/Date---Time-Input.html)