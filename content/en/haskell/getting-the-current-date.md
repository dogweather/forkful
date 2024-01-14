---
title:    "Haskell recipe: Getting the current date"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

In today's fast-paced world, having access to the current date is a crucial aspect of any programming language. Whether you need to keep track of deadlines, schedule tasks, or simply display the date to users, being able to get the current date is an essential skill for any programmer.

## How To

Getting the current date in Haskell is a fairly simple process. There are multiple ways to achieve this, depending on your specific needs. Let's take a look at a few examples:

```Haskell
-- Importing the Data.Time library
import Data.Time

-- Using getCurrentTime function to get the current date and time
getCurrentTime >>= return . utctDay

-- Output: 2021-05-24

-- Using the getZonedTime function to get the current date and time in a specific timezone
getZonedTime >>= return . zonedTimeToLocalTime

-- Output: 2021-05-24 13:35:00.26772 UTC

-- Using the formatTime function to format the date in a specific way
formatTime defaultTimeLocale "%A, %B %d, %Y" (zonedTimeToLocalTime <$> getZonedTime)

-- Output: Monday, May 24, 2021
```

As you can see, the Data.Time library provides us with the necessary functions to get the current date and time, as well as format it in a specific way. It is important to note that the date and time will be based on the system clock, so it is always accurate.

## Deep Dive

If you're interested in digging deeper into the topic of getting the current date in Haskell, you can explore the various functions and types available in the Data.Time library. You can also learn about the different time zones and how they affect the output of the functions. Additionally, you can look into formatting options and customizing the output of the current date according to your needs.

It is worth noting that the Data.Time library has some limitations, such as not being able to handle dates before 1600 or after 9999. If you need to work with dates outside of this range, you can explore other libraries such as the time library.

## See Also

Here are some helpful resources for further exploring the topic of getting the current date in Haskell:

- [Data.Time documentation](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskell Date and Time tutorial](https://www.tutorialspoint.com/haskell/haskell_date_time.htm)
- [Time library for handling dates outside of 1600-9999 range](https://hackage.haskell.org/package/time-1.9/docs/Data-Time.html)