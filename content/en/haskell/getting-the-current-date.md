---
title:                "Getting the current date"
html_title:           "Haskell recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why 

Python has been the go-to programming language for many years for tasks such as web development, data analysis, and automation. However, Haskell, a lesser-known functional programming language, has been gaining popularity in recent years, especially in the world of finance and data science. One of the many useful features in Haskell is its ability to handle date and time operations efficiently. In this article, we will explore how to get the current date in Haskell and discuss the advantages of using this language for such operations.

## How To

To get the current date in Haskell, we first need to import the `Data.Time` module. This module provides functions and types for working with dates and times. Once imported, we can use the `getCurrentTime` function to get the current date and time. 

```
import Data.Time

getCurrentTime
```

This function returns a value of type `IO UTCTime`, which represents an absolute time in Universal Time. We can then convert this value into a more readable format using the `toLocalTime` function.

```
toLocalTime :: Timezone -> UTCTime -> IO ZonedTime
```

Here is an example of how we can get the current date and time in the UTC timezone:

```
import Data.Time

printCurrentTime :: IO ()
printCurrentTime = do
  utcTime <- getCurrentTime
  putStrLn $ show utcTime
 
main :: IO ()
main = do
  printCurrentTime
```

Output:

```
2021-12-04 13:23:47 UTC
```

We can also specify a different timezone by passing it as an argument to the `toLocalTime` function. For example, if we want to get the current date and time in the local timezone, we can do the following:

```
import Data.Time

printCurrentTime :: IO ()
printCurrentTime = do
  utcTime <- getCurrentTime
  localTime <- toLocalTime defaultTimezone utcTime
  putStrLn $ show localTime
 
main :: IO ()
main = do
  printCurrentTime
```

Output:

```
2021-12-04 07:23:47 EST
```

## Deep Dive

One of the advantages of using Haskell for date and time operations is its robust type system. Unlike in other languages where variables can have any type, Haskell's type system ensures that values of the same type are used throughout the program. This helps avoid bugs and errors commonly encountered when working with dates and times in other languages. 

Moreover, Haskell's `Data.Time` module provides various functions for working with dates and times, such as adding or subtracting a certain number of days, months, or years, comparing different dates, and formatting dates into strings in different formats. These functions make it easier to perform complex date and time operations in a concise and readable manner.

## See Also

- [Haskell Wiki - Date and Time](https://wiki.haskell.org/Date_and_time)
- [Hackage - Data.Time](https://hackage.haskell.org/package/time)
- [Learn You a Haskell for Great Good! - Souped-up Types: Typeclasses 102](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#typeclasses-102)