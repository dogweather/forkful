---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Haskell String Length: A Simple Guide

## What & Why?
Finding the length of a string is to determine how many characters it contains. It’s a pretty common task in programming, useful in scenarios like data validation and resource allocation.

## How to:
Finding the length of a string in Haskell is straightforward. You use the `length` function. Here's how.

```Haskell
main = do
    let str = "Hacker News"
    print(length str)
```
When you run the program, the output would be:
```Haskell
11
```

In this case, the string `"Hacker News"` is composed of 11 characters, including the space.

## Deep Dive
Historically, measuring the length of a string was crucial to manage memory. Today, although high-level languages handle most of this for us, it remains an essential skill. 

If efficiency is your concern, beware - Haskell's `length` function can be costly for large strings since it must traverse the entire string. An alternative approach can be using the `Data.Text.length` function that works on strict and lazy Text. But remember, it counts Unicode characters, not bytes. 

```Haskell
import qualified Data.Text as T
main = do
    let str = T.pack "Haskell"
    print(T.length str)
```
Implementation speaking, Haskell's length function operates on Lists, which Strings are just a type of. It's a pure function without side effects that counts the items in a list till it reaches an empty list (`[]`).

## See Also
1. [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch18.html)
2. [Real World Haskell, Chapter 6: Using Typeclasses](http://book.realworldhaskell.org/read/using-typeclasses.html)
3. [Haskell Wiki: Length of a List](https://wiki.haskell.org/How_to_determine_the_length_of_a_list)
4. [Introduction to Haskell: Lazy Evaluation](http://intro.cs.auckland.ac.nz/Y.2019/CS1PPT/#/3/22)