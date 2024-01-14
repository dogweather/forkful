---
title:                "Haskell recipe: Converting a date into a string"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
While working with dates in programming, it is often necessary to convert them into strings for better readability or to display them in a specific format. This is especially important when dealing with dates in user interfaces or reports.

## How To
To convert a date into a string in Haskell, we can use the `formatTime` function from the `Data.Time.Format` module. This function takes in two arguments - a format string and a date value, and returns a string representing the formatted date.

Let's take a look at an example:

```Haskell
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar

-- Create a date value using the fromGregorian function
myDate = fromGregorian 2021 05 21

-- Format the date into a string using the formatTime function
output = formatTime defaultTimeLocale "%d %b, %Y" myDate

-- Print the output
print output
```

Running this code will output: `21 May, 2021` which is the formatted date in the specified format.

The `formatTime` function also allows us to use different formatting options such as displaying the day with or without leading zeros, showing the time, and more. These options are specified in the format string by using placeholders for different date components, such as `%d` for the day, `%m` for the month, and so on.

## Deep Dive
Behind the scenes, the `formatTime` function uses the `toGregorian` function to convert the date value into its corresponding year, month, and day components. It then applies these components to the specified format string to generate the desired output.

It is worth noting that the format string is dependent on the current locale settings of the system. This means that the output of the function may differ based on the language or region settings of the system.

## See Also
- [Hackage - formatTime documentation](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime)
- [Learn You a Haskell - Dates and Times](http://learnyouahaskell.com/dates-and-times)