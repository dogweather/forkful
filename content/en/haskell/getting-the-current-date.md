---
title:                "Haskell recipe: Getting the current date"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

In today's digital age, it's important to be able to accurately keep track of time and dates. Whether you're building a scheduling app, creating a time-sensitive program, or simply curious about the current date, knowing how to get the current date in Haskell can be incredibly useful. In this blog post, we'll explore the different methods for obtaining the current date in Haskell.

## How To

To get the current date in Haskell, we can use the `getCurrentTime` function from the `Data.Time.Clock` module. This function returns the current time in the `IO` monad, which means it can interact with the outside world. Here's an example of how we can use this function:

```Haskell
import Data.Time.Clock

main :: IO ()
main = do
  currentTime <- getCurrentTime
  putStrLn $ "The current date is: " ++ show (utctDay currentTime)
```

Running this code will print out the current date in the format `YYYY-MM-DD`. For example, if today's date is January 1st, 2022, the output would be `The current date is: 2022-01-01`.

We can also use the `getZonedTime` function from the `Data.Time.LocalTime` module to get the current time in a specific time zone. Here's an example:

```Haskell
import Data.Time.LocalTime

main :: IO ()
main = do
  currentTime <- getZonedTime
  putStrLn $ "The current time in New York is: " ++ show (zonedTimeToLocalTime currentTime)
```

This code will print out the current time in New York. Keep in mind that the time zone will depend on your system settings. You can also use the `getCurrentTimeZone` function to get the current time zone.

## Deep Dive

Under the hood, the `getCurrentTime` function uses the POSIX `gettimeofday` function to get the current time in seconds and microseconds since 1970-01-01 00:00:00 UTC. It then converts this to a `UTCTime` data type in Haskell. This allows us to perform various operations on the time, such as adding or subtracting seconds, minutes, or even days.

We can also use the `formatTime` function from the `Data.Time.Format` module to format the current time in a specific way. For example, if we want to print out the current date in the format `Day, Month Year`, we can use the following code:

```Haskell
import Data.Time.Clock
import Data.Time.Format
import System.Locale

main :: IO ()
main = do
  currentTime <- getCurrentTime
  putStrLn $ "The current date is: " ++ formatTime defaultTimeLocale "%A, %B %e %Y" (utctDay currentTime)
```

This code will print out the current date in a more readable format, such as `Sunday, January 1 2022`.

## See Also

- [Haskell Docs: Data.Time.Clock](https://hackage.haskell.org/package/time/docs/Data-Time-Clock.html)
- [Haskell Docs: Data.Time.LocalTime](https://hackage.haskell.org/package/time/docs/Data-Time-LocalTime.html)
- [Learn You a Haskell: Input and Output](http://learnyouahaskell.com/input-and-output)