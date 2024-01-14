---
title:    "Elm recipe: Comparing two dates"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

When working with dates in any programming language, it is important to have a way to compare them. This allows for efficient sorting and organizing of data. In this blog post, we will explore how to compare two dates in Elm.

## How To

To compare two dates, we will use the built-in `compare` function in Elm. This function takes in two values and returns an `Order` type which can be either `LT` (less than), `EQ` (equal to), or `GT` (greater than).

```Elm
compare : comparable -> comparable -> Order
```

Let's say we have two date values: `date1` and `date2`. We can compare them using the `compare` function as follows:

```Elm
result = compare date1 date2
```

The `result` variable will now hold either `LT`, `EQ`, or `GT` depending on the relationship between `date1` and `date2`.

We can also use this function in conditional statements to perform certain actions based on the comparison result. For example:

```Elm
if result == LT then
    -- do something
else if result == EQ then
    -- do something else
else
    -- do another thing
```

## Deep Dive

Under the hood, the `compare` function in Elm uses the `comparable` typeclass. This means that any data type that supports comparison can be used with this function.

When comparing dates, we need to make sure that they are in a consistent format. The `Date` module in Elm offers functions to convert a `Time.Posix` value to a `Date` type. This allows us to easily convert timestamps into dates for comparison.

## See Also

- [Official documentation for compare function in Elm](https://package.elm-lang.org/packages/elm/core/latest/Basics#compare)
- [Date module in Elm](https://package.elm-lang.org/packages/elm/time/latest/Time-Date)
- [Blog post on working with dates in Elm](https://dev.to/bersam/working-with-dates-in-elm-jm6)