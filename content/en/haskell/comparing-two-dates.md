---
title:    "Haskell recipe: Comparing two dates"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why
Have you ever needed to compare two dates in your Haskell program? Maybe you're working on a project that involves scheduling events or tracking time-sensitive data. In these cases, it's useful to be able to compare dates to see which one comes first. In this blog post, we'll explore how to compare dates in Haskell and why it's a handy skill to have in your programming toolkit.

## How To
First, let's define two dates using the `Data.Time` module in Haskell. For this example, we'll use the `LocalDate` type.

```Haskell
date1 :: LocalDate
date1 = fromGregorian 2021 9 20

date2 :: LocalDate
date2 = fromGregorian 2021 9 25
```

Next, we can use the `compare` function, which takes two arguments and returns an `Ordering` data type. This data type can have three possible values: `GT` (greater than), `LT` (less than), or `EQ` (equal).

```Haskell
compareDates :: LocalDate -> LocalDate -> Ordering
compareDates date1 date2 = compare date1 date2
```

We can then use pattern matching to see the result of the comparison and print out a message accordingly.

```Haskell
printComparison :: LocalDate -> LocalDate -> IO ()
printComparison date1 date2 = case compareDates date1 date2 of
                                GT -> putStrLn "Date 1 is after Date 2."
                                LT -> putStrLn "Date 1 is before Date 2."
                                EQ -> putStrLn "Date 1 is equal to Date 2."
```

Let's try it out with our two example dates:

```
> printComparison date1 date2
Date 1 is before Date 2.
```

## Deep Dive
Under the hood, the `LocalDate` type is actually just an alias for the `Day` type, which represents a day in the Gregorian calendar. So when we use the `fromGregorian` function, we're essentially creating a `Day` value. The `compare` function then compares the two `Day` values and returns the appropriate `Ordering`.

It's also worth noting that the `LocalDate` type is part of the `time` package, which is not included in the standard library. Therefore, you will need to install this package using a package manager like Cabal or Stack before using it in your code.

## See Also
- [Official Haskell documentation for the `time` package](https://hackage.haskell.org/package/time)
- [Tutorial on working with dates and times in Haskell](https://www.tutorialspoint.com/haskell/haskell_dates.htm)
- [Haskell date comparison function on Rosetta Code](https://rosettacode.org/wiki/Date_comparison#Haskell)