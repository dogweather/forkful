---
title:                "Haskell recipe: Calculating a date in the future or past"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Blog post

## Why

Have you ever found yourself struggling to figure out what day of the week your birthday will fall on next year? Or maybe you need to schedule an important event on a specific date in the future. What if I told you that with just a few lines of code, you could effortlessly calculate any date in the future or past? That's right, in this blog post, we'll be diving into the world of date calculations in Haskell and show you just how powerful and useful they can be.

## How To

First, let's start by importing the `Data.Time` library, which provides us with various functions for working with dates and times. We'll also need to enable the `OverloadedStrings` extension to make working with dates easier.

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Time
```

Now, let's say we want to calculate today's date and print it in a more readable format. We can do this by using the `getCurrentTime` function to get the current `UTCTime` and then using the `utcToLocalTime` function to convert it into our local time.

```Haskell
-- Get current time
currentTime <- getCurrentTime

-- Convert to local time
let currentTimeLocal = utcToLocalTime (TimeZone (0) False "UTC") currentTime

-- Print date in "DD/MM/YYYY" format
putStrLn $ formatTime defaultTimeLocale "%d/%m/%Y" currentTimeLocal
```

This should give us the current date in the format of "DD/MM/YYYY". Easy, right? But what if we want to calculate a date in the future or past? For that, we can use the `addDays` function, which takes two arguments - the number of days we want to add (can be negative for past dates) and the starting date.

```Haskell
-- Calculate the date 10 days from today
let futureDate = addDays 10 currentTimeLocal

-- Print future date in "DD/MM/YYYY" format
putStrLn $ formatTime defaultTimeLocale "%d/%m/%Y" futureDate
```

The output of the above code should be 10 days from the current date. Similarly, we can also calculate dates in the past by providing a negative number of days.

## Deep Dive

Behind the scenes, Haskell uses the proleptic Gregorian calendar for its date calculations. This means that it assumes the Gregorian calendar was always in effect, even for dates before it was officially introduced in 1582. However, there are other libraries such as `Data.Dates` that provide support for non-proleptic calendars like Julian and Roman.

Haskell also comes with a `DiffTime` data type, which represents time durations in seconds. This makes it easy to perform calculations such as adding or subtracting hours, minutes, or seconds to a date.

There are many other useful functions in the `Data.Time` and `Data.Dates` libraries that can help you with your date calculations. With some practice, you'll become a pro at it in no time!

## See Also

- [Documentation for Data.Time library](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Blog post on working with dates in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/dates)
- [Documentation for Data.Dates library](https://hackage.haskell.org/package/dates/docs/Data-Dates.html)

So go ahead and have some fun with date calculations in Haskell. Happy coding!