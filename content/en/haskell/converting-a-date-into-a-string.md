---
title:    "Haskell recipe: Converting a date into a string"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why

Converting dates into strings is a common task in many programming languages, including Haskell. It allows developers to format the date in a specific way to display to users or to manipulate within the program itself. In this blog post, we will explore how to convert a date into a string using Haskell.

## How To

To convert a date into a string in Haskell, we can use the `formatTime` function from the `Data.Time.Format` module. This function takes in a format string and a `UTCTime` value and returns a string representation of the date in the specified format.

Let's take a look at an example:

```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)

main = do
  -- Get the current UTC time
  currentTime <- getCurrentTime

  -- Format the time as an ISO 8601 date string
  let dateString = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" currentTime

  -- Print the result
  putStrLn dateString
```

In this example, we first import the necessary modules, `Data.Time.Format` and `Data.Time.Clock`. Then, we use the `getCurrentTime` function to get the current UTC time. Next, we pass in a format string and the current time to the `formatTime` function.

The format string `"%Y-%m-%dT%H:%M:%SZ"` specifies the desired format, which in this case is ISO 8601. Finally, we print the resulting string to the console, which will look something like this: `2021-02-15T13:20:00Z`.

There are many other format options available, and you can customize the format string to suit your needs. Some common options include `%Y` for the full year, `%m` for the month, `%d` for the day, and `%H` for the hour.

## Deep Dive

Behind the scenes, the `formatTime` function uses the `TimeLocale` data type to determine how to format the date string. This type contains various formatting options for different locales and also allows for custom formatting.

Additionally, the `UTCTime` type represents a point in time in UTC and is used to calculate the date and time that will be displayed. It is important to keep in mind that the formatting options may differ depending on the time zone being used.

## See Also

- [Haskell documentation on `Data.Time.Format`](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [Tutorial on working with dates in Haskell](https://stackoverflow.com/questions/12566493/how-do-i-work-with-dates-in-haskell)
- [ISO 8601 format specifications](https://www.iso.org/iso-8601-date-and-time-format.html)