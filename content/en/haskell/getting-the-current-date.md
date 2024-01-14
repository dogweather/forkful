---
title:    "Haskell recipe: Getting the current date"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why
Have you ever needed to program something that relies on the current date? Whether it's creating a to-do list or filtering data based on time, having access to the current date in your code can be incredibly useful.

## How To
In Haskell, there are a few different ways to get the current date. One of the most commonly used methods is by importing the `Data.Time` module. Let's take a look at an example:

```Haskell
import Data.Time

main = do
  currentTime <- getCurrentTime
  putStrLn $ "The current date and time is: " ++ show currentTime
```

In this code, we import the `Data.Time` module, which provides functions for working with dates and times. Then, we use the `getCurrentTime` function to get the current date and time and store it in the `currentTime` variable. Finally, we use `putStrLn` to print out the current date and time to the console.

If we run this code, we should see something like this:

```
The current date and time is: 2021-04-11 18:34:26.637424 UTC
```

But what if we just want to get the current date without the time? We can use the `getCurrentDay` function instead:

```Haskell
import Data.Time

main = do
  currentDate <- getCurrentDay
  putStrLn $ "Today's date is: " ++ show currentDate
```

This time, we see just the date without the time in the output:

```
Today's date is: 2021-04-11
```

## Deep Dive
Under the hood, the `getCurrentTime` and `getCurrentDay` functions are using the `IO` monad to perform side effects and retrieve the current date from the system clock. This allows us to use them in our code without worrying about the complexities of working with dates and times.

Additionally, the `Data.Time` module provides many other functions for manipulating and formatting dates and times, such as `addUTCTime` and `formatTime`. By exploring the documentation, we can find the best functions for our specific needs.

## See Also
- [Data.Time module documentation](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Official Haskell website](https://www.haskell.org/)
- [Learn You a Haskell tutorial](http://learnyouahaskell.com/)

Now that you have the tools to get the current date in Haskell, you can start incorporating it into your code and take advantage of the power and flexibility of functional programming. Happy coding!