---
title:                "Finding the length of a string"
html_title:           "Haskell recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string is a common task in programming, which basically means determining the number of characters in a given sequence of characters. This is particularly useful for tasks such as data validation, formatting, and indexing. It allows programmers to manipulate and use strings in various ways to achieve their desired outcome.

## How to:

To find the length of a string in Haskell, we can use the ```length``` function. It takes in a string as its parameter and returns an integer representing the number of characters in the string. Let's see an example:

```Haskell
length "Hello World"
```
This will return the value of 11, as there are 11 characters in the string "Hello World".

We can also use the ```length``` function on a variable that contains a string, like this:

```Haskell
let greeting = "Hello World"
length greeting
```

The output will be the same as the previous example. Additionally, we can also use the ```length``` function on an empty string, which will return the value of 0.

```Haskell
length ""
```

## Deep Dive:

The ```length``` function in Haskell is actually a part of the standard library, specifically the Data.List module. It is a recursive function that counts the number of values in a list until it reaches the empty list, which is represented by an empty string.

An alternative way to find the length of a string is by using pattern matching. We can define our own function that takes in a string as its parameter and recursively counts the number of characters until it reaches an empty string.

```Haskell
stringLength :: [Char] -> Int
stringLength "" = 0
stringLength (_:xs) = 1 + stringLength xs
```

This function will return the same output as the ```length``` function and can serve as an exercise to understand recursion better.

## See Also:

To learn more about the ```length``` function and other string manipulation functions in Haskell, refer to the [official documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html) for the Data.List module.

For a more detailed explanation and comparison of different ways to find the length of a string in Haskell, check out this [blog post](https://functional.christmas/2019/25) by functional programmer Oskar Wickström.