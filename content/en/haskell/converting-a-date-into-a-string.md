---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# The Haskell Date-to-String Express: Making Dates Chatty

## What & Why?

Converting a date into a string in Haskell means turning a date object into a format we humans can easily read. It's done to display dates on screens, in logs, serialize them for storage, or even when debugging your code.

## How to:

Let's make things practical. Here's how to convert a date object to a string in Haskell:

```Haskell
import Data.Time

main :: IO ()
main = do
  currentTime <- getCurrentTime
  print currentTime
  putStrLn $ formatTime defaultTimeLocale "%B %e, %Y" currentTime
```

Run it, and you'll see:

```Shell
2022-03-28 14:20:00 UTC
March 28, 2022
```

Here we use `getCurrentTime`, a function from the `Data.Time` package that gets the current date and time. Then `formatTime` is used with `defaultTimeLocale` to format this date-time value into a more legible one: Month Day, Year". 

## Deep Dive

Historically, Haskell didn't have a robust library for working with dates and times, and developers had to resort to using plugins with the C language or work around it with complex, difficult-to-maintain code.

Fortunately, the situation has improved. We're lucky to have the `Data.Time` library, seen above, which makes handling time in Haskell a lot simpler. 

Meanwhile, more options are available. There's the `thyme` package, if `Data.Time` feels too heavy, or `time-lens` for those preferring a lens-like syntax. 

Lastly, about implementing the conversion ourselves: Honestly, save your time. Use a library. It's (mostly) about code reuse. Besides, date and time handling is a complex subject often underestimated, and the established libraries have had much effort invested into testing, debugging, and making things time-zone-safe.

## See Also

Get started with more time in Haskell:

- [The Data.Time Package Hackage](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [The Thyme Package](https://hackage.haskell.org/package/thyme)
- [The Time-lens Package](https://hackage.haskell.org/package/time-lens)
- 'Lies, Damned Lies and Datetimes' Blog Post by Henrik Hjelt: [https://baatz.io/posts/lies-damned-lies-and-datetimes/](https://baatz.io/posts/lies-damned-lies-and-datetimes/)

Remember, exploring is the key to mastery. Happy coding!