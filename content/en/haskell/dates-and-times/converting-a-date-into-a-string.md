---
date: 2024-01-20 17:36:55.678499-07:00
description: 'How to: In Haskell, you use the `formatTime` function from the `Data.Time.Format`
  module for this job. Let''s dive right into some code.'
lastmod: '2024-03-13T22:45:00.138919-06:00'
model: gpt-4-1106-preview
summary: In Haskell, you use the `formatTime` function from the `Data.Time.Format`
  module for this job.
title: Converting a date into a string
weight: 28
---

## How to:
In Haskell, you use the `formatTime` function from the `Data.Time.Format` module for this job. Let's dive right into some code:

```haskell
import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)

main :: IO ()
main = do
    -- Grab the current time
    currentTime <- getCurrentTime
    let currentZone = utc
        -- Convert UTC time into a local time object
        localTime = utcToLocalTime currentZone currentTime
        -- Format the date as "YYYY-MM-DD"
        dateString = formatTime defaultTimeLocale "%F" localTime
    putStrLn dateString
```

And here's what you might see as output, depending on the current date:

```
2023-04-01
```

## Deep Dive
Dating back to the early days of programming, converting dates to strings has always been a matter of practical usability. In Haskell, we owe our date and time handling to the `Data.Time` library, which was inspired by the functionality and improvements over older libraries such as `old-time`.

There are alternatives to `formatTime`, like using `show` to convert a date to a string directly, but this won't give you custom formatting options. The `formatTime` function is rich, supporting a variety of formats that align with the C's `strftime` function patterns. It's flexible and locale-aware, using `defaultTimeLocale` or other locales to format dates according to cultural conventions.

Regarding implementation, the `Data.Time.Format` functions are pure, meaning they don't rely on or cause side effects. This aligns with Haskell's functional programming ethos, which aims for functions to be predictable and their outcomes determined only by their inputs.

## See Also
For more extensive work on dates and times in Haskell, peruse the following:

- The `Data.Time` module documentation: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- Details about `strftime` format strings, which `formatTime` imitates: [http://man7.org/linux/man-pages/man3/strftime.3.html](http://man7.org/linux/man-pages/man3/strftime.3.html)
- Haskell's approach to IO and purity: [https://www.haskell.org/tutorial/io.html](https://www.haskell.org/tutorial/io.html)
