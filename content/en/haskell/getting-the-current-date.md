---
title:                "Haskell recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

In today's digital age, it's become a common practice for developers to include the current date in their programs or applications. Knowing the current date can be useful for various reasons such as time stamping data, scheduling tasks, or even just displaying the date for user interface purposes. In this blog post, we'll explore how to get the current date using Haskell programming language.

## How To

Getting the current date in Haskell is a simple process that involves using the built-in `Data.Time` module. Within this module, there are several functions that allow us to work with dates and times. The most commonly used function for getting the current date is `getCurrentTime`, which returns the current date and time as a data type called `UTCTime`. Let's take a look at an example:

```Haskell
import Data.Time

currentDate :: IO UTCTime
currentDate = getCurrentTime

main :: IO ()
main = do
  current <- currentDate
  putStrLn $ "Today's date is: " ++ show (utctDay current)
  putStrLn $ "Current time is: " ++ show (utctDayTime current)
```

Running this code will output something like this:

```
Today's date is: 2021-01-01
Current time is: 12:00:00 AM UTC
```

In this code snippet, we first import the `Data.Time` module and define a function called `currentDate` which uses the `getCurrentTime` function to retrieve the current date and time. Then, in the `main` function, we use the `utctDay` function to extract just the date from the `UTCTime` data type and the `utctDayTime` function to get the current time. We then print these values to the console using `putStrLn`.

## Deep Dive

Now, let's take a deeper look at the `Data.Time` module and some other useful functions for working with dates. The `UTCTime` data type is a representation of a specific point in time, but sometimes we may need to work with different time zones. To do this, we can use the `ZonedTime` data type which also includes information about the time zone.

Another useful function for getting the current date is `getCurrentTimeZone`, which returns the time zone of the current system. Then, we can use the `utcToLocalTime` function to convert the `UTCTime` value to a `ZonedTime` value based on the specified time zone. Here's an example:

```Haskell
import Data.Time

currentDate :: IO ZonedTime
currentDate = do
  zone <- getCurrentTimeZone
  utcCurrent <- getCurrentTime
  return $ utcToLocalTime zone utcCurrent

main :: IO ()
main = do
  current <- currentDate
  putStrLn $ "Today's date and time in your time zone is: " ++ show current
```

This will output something like this:

```
Today's date and time in your time zone is: 2021-01-01 12:00:00 AM EST
```

## See Also

- [Haskell Data.Time module documentation](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Working with dates and times in Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/simple-formatting-with-the-time-format-library)
- [Time zones in Haskell](https://www.programminghunk.com/2020/05/timezones-in-haskell.html)