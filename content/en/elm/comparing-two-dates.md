---
title:    "Elm recipe: Comparing two dates"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

Comparing dates is a common task in programming, especially when working with data or creating dynamic applications. In Elm, comparing dates can be done easily and efficiently with the help of built-in functions. In this blog post, we will explore why comparing dates is important and how to do it in Elm.

## How To

To compare two dates in Elm, we can use the `compare` function from the `Date` module. This function takes two `Date` values and returns an `Order` value, which can be `LT` (less than), `GT` (greater than), or `EQ` (equal).

Let's see an example of comparing two dates in Elm:

```Elm
import Date exposing (compare)

date1 = Date.fromYMD 2020 11 15
date2 = Date.fromYMD 2019 06 20

result = compare date1 date2

-- result will be GT since date1 is after date2
```

In the above code, we first import the `Date` module which contains the `compare` function. Then, we create two `Date` values using the `fromYMD` function, which takes in the year, month, and day as arguments. After that, we call the `compare` function and pass in the two dates as arguments, which returns an `Order` value. In this case, the result will be `GT` since date1 is after date2.

We can also use the `==` and `<` operators to compare dates in Elm. The `==` operator checks if two dates are equal, while the `<` operator checks if one date is before the other.

```Elm
date1 == date2 -- returns False
date1 < date2  -- returns False since date1 is after date2
```

## Deep Dive

Dates in Elm are represented using the `Date` type, which is an alias for `Posix`. This type is a number of milliseconds since epoch (January 1, 1970). This makes comparing dates in Elm very efficient since the comparison is done on simple numbers rather than complex data structures.

One important thing to note is that when using `fromYMD` to create dates, the month argument is 0-indexed, meaning January is represented as 0 and December as 11. This can be a bit confusing at first, but it follows the same convention as other programming languages and is easily adjustable.

## See Also

If you want to learn more about dates and time in Elm, check out these resources:

- [Elm Date and Time module documentation](https://package.elm-lang.org/packages/elm/time/latest/Date) 
- [Date and Time basics in Elm tutorial](https://guide.elm-lang.org/effects/time.html)
- [Date and Time in Elm video tutorial](https://www.youtube.com/watch?v=5TxiCK_L8Hs&t=1s)