---
title:    "Haskell recipe: Converting a date into a string"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why 

Have you ever needed to display a date in a specific format in your Haskell program? Converting a date value into a string is a common task for developers, and knowing how to do it efficiently can save you time and headaches in your coding journey. In this post, we will explore the process of converting a date into a string in Haskell.

## How To

Converting a date into a string in Haskell is a straightforward process. The `Data.Time` module provides functions to handle date and time values. The `formatTime` function takes in a formatting string and a date value and returns a string representing the date in the specified format. 

```Haskell
import Data.Time

date :: Day
date = fromGregorian 2021 7 14  -- represents July 14th, 2021

formatTime defaultTimeLocale "%B %d, %Y" date
-- output: "July 14, 2021"
```

In this example, we used the `%B` placeholder for full month name, `%d` for day of month with zero padding, and `%Y` for the full year. You can use various placeholders to customize the format according to your needs. Check out the [Time library documentation](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html) for a complete list of placeholders and their meanings.

## Deep Dive

Behind the scenes, the `formatTime` function uses the `Text.Printf` module to handle the formatting string. The formatting string follows similar conventions as the `printf` function from C language, with placeholders denoted by a `%` symbol. The `defaultTimeLocale` is a predefined value in the `Data.Time` module that provides a standard format for dates and times. However, you can also create your own custom time locale to use for formatting.

Another useful function in the `Data.Time` module is `parseTimeM`, which takes in a formatting string and a string value representing a date and returns a `Maybe` date value. This function comes in handy when you need to parse a string into a date value, such as when you need to process user input.

## See Also

- [Time library documentation](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [Printf library documentation](https://hackage.haskell.org/package/base/docs/Text-Printf.html#v:printf)

Now that you know how to convert a date into a string in Haskell, you can easily display date values in your desired format in your programs. With the help of the available functions and the knowledge of formatting strings, you can handle date and time values efficiently in your code.