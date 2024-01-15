---
title:                "Calculating a date in the future or past"
html_title:           "Haskell recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
You might be planning an event or trying to figure out when a project will be completed, and need to know the date in the future or past. Calculating dates can be tedious and error-prone, but with Haskell's built-in date library and powerful functional programming capabilities, it can be made easier and more accurate.

## How To
To calculate a date in the future or past, we first need to import the `Data.Time` library. We can do this by adding the following line to the top of our code:

```Haskell
import Data.Time
```

Next, we need to create a `Day` object representing the starting date. We can do this by using the `fromGregorian` function, which takes in the year, month, and day as arguments. For example, if we want to calculate a date 10 years in the future from today, we can write:

```Haskell
let today = fromGregorian 2021 12 1
```

Now, to find the date 10 years in the future, we can use the `addGregorianYearsClip` function, which will return a new `Day` object representing the calculated date. We can write:

```Haskell
let futureDate = addGregorianYearsClip 10 today
```

Finally, we can use the `show` function to display the date in a readable format. Our code should look like this:

```Haskell
import Data.Time

main = do
    let today = fromGregorian 2021 12 1
    let futureDate = addGregorianYearsClip 10 today
    putStrLn $ "The date 10 years from today is: " ++ show futureDate
```

Running this code will output:

```
The date 10 years from today is: 2031-12-01
```

To calculate a date in the past, we can use the `addGregorianYearsClip` function with a negative value for the years. Other functions such as `addGregorianMonthsClip` and `addGregorianDays` can also be used to calculate dates based on months and days. This allows for flexibility in calculating dates based on specific requirements.

## Deep Dive
The `Data.Time` library offers many functions for working with dates and times. These include functions for converting between different time zones, extracting specific information such as the day of the week or the current time, and formatting dates in different ways.

One important concept when working with dates in Haskell is the `DiffTime` type. This type represents a duration of time in seconds and is used in many date and time calculations. It is recommended to become familiar with this type when working with dates in Haskell.

Additionally, the `UTCTime` type is used to represent a specific point in time in Universal Time Coordinated (UTC). This type can be converted to and from the `LocalTime` type, which represents a specific point in time in a specific time zone. These types are useful when working with timestamps and handling time differences between different locations.

## See Also
- [Haskell Data.Time Documentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Learn You a Haskell for Great Good! - Dates and Time](http://learnyouahaskell.com/for-great-good-time#datetime)
- [Real World Haskell - Date and Time](http://book.realworldhaskell.org/read/data-and-types.html#data-types.type-synonyms-and-type-literals)