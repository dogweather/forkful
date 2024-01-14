---
title:                "Haskell recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to display a date in a specific format in your Haskell program? Maybe you want to display the date as a string, such as "December 1, 2021" or "12/1/21". In order to do this, you will need to convert a date into a string. In this blog post, we'll explore how to do just that in Haskell.

## How To

To convert a date into a string in Haskell, we will use the `formatTime` function from the `Data.Time.Format` module. First, let's import the necessary modules:

```Haskell
import Data.Time.Format
import Data.Time.LocalTime
```

Next, we will create a date value using the `fromGregorian` function from the `Data.Time.Calendar` module. This function takes in three arguments: the year, month, and day.

```Haskell
let myDate = fromGregorian 2021 12 1
```

Now, we can use the `formatTime` function to convert our date into a string. This function takes in two arguments: a string that represents the desired format and the date value we created above.

```Haskell
let dateString = formatTime defaultTimeLocale "%B %e, %Y" myDate
```

In this example, we are using the `defaultTimeLocale` which represents the standard time format in your current location. The `%B` and `%e` are formatting codes that will be replaced with the month and day of the date respectively. The `%Y` represents the four-digit year.

Now, let's print out the `dateString` variable to see our converted date:

```Haskell
putStrLn dateString
```

< Output: December 1, 2021 >

You can try playing around with different formatting codes to achieve the desired date format.

## Deep Dive

Under the hood, the `formatTime` function uses the `showTime` function which converts the date value into the `String` type. The `showTime` function has the following type signature:

```Haskell
showTime :: FormatTime t => t -> TimeLocale -> String -> String
```

As you can see, it takes in three arguments similar to the `formatTime` function. The `FormatTime` class is defined in the `Data.Time.Format` module and contains different instances for various data types, such as `UTCTime` and `ZonedTime`.

The `TimeLocale` is a record type that contains information about the specific date and time formats in your location. It is used to customize the output of the `formatTime` function.

## See Also

- [Hackage - Data.Time.Format](http://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [Hackage - Data.Time.LocalTime](https://hackage.haskell.org/package/time/docs/Data-Time-LocalTime.html)
- [Haskell Wiki - Time and Date](https://wiki.haskell.org/Time_and_Date)