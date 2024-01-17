---
title:                "Converting a date into a string"
html_title:           "Haskell recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date into a string means taking a date value, such as 2020-05-20, and converting it into a string, such as "May 20th, 2020". This is a common task in programming, as it allows dates to be displayed in a readable format for users, and can also be used for data manipulation and storage purposes.

## How to:
To convert a date into a string in Haskell, we can use the `formatTime` function from the `time` package. This function takes in a format string and a date value, and returns a string representation of the date in the specified format.

Example code:
```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime, getCurrentTime)
 
main :: IO ()
main = do
  currentTime <- getCurrentTime
  let dateString = formatTime defaultTimeLocale "%B %e, %Y" currentTime
  putStrLn dateString
```

Output:
```
May 20, 2020
```

In the above code, we import the necessary modules from the `time` package and use the `getCurrentTime` function to get the current time as a `UTCTime` value. We then pass in this value and a format string to the `formatTime` function, which returns a string representation of the current time in the specified format.

## Deep Dive:
Converting a date into a string has been a common task in programming for many years, as dates are an essential part of many applications. In older languages, such as C and Java, date formatting was often a tedious and error-prone process. However, in Haskell, the `time` package provides a simple and straightforward way to convert dates to strings.

An alternative to using the `formatTime` function is to use the `timeShow` function from the `time-locale-compat` package. This function takes in a format string and a date value and returns a string representation of the date in the specified format. The advantage of using this function is that it follows the locale-specific date and time formatting rules, which may be necessary for internationalization purposes.

Internally, both the `formatTime` and `timeShow` functions use the `Text` type to represent strings, which allows for efficient and safe manipulation of text in Haskell.

## See Also:
- [Hackage documentation for the `time` package](https://hackage.haskell.org/package/time)
- [Hackage documentation for the `time-locale-compat` package](https://hackage.haskell.org/package/time-locale-compat)
- [Haskell.org's official tutorial on working with dates and times](https://www.haskell.org/tutorial/datetime.html)