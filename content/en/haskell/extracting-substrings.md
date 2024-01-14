---
title:                "Haskell recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
Substring extraction is a common task in programming, and luckily, Haskell provides us with an easy and efficient way to do it. By extracting substrings, we can manipulate and work with specific parts of a larger string, making our programs more dynamic and flexible.

## How To
To extract a substring in Haskell, we will be using the `take` and `drop` functions. Let's say we have the string "Hello World" and we want to extract the substring "World". We can do this by using the following code:
```Haskell
let myString = "Hello World"
let mySubstring = drop 6 myString
```
In the code above, we first define the original string, "Hello World", and then use the `drop` function to drop the first 6 characters, leaving us with the substring "World". 

We can also use the `take` function to extract a substring. For example, if we want to get the first 5 characters of our string, we can use the following code:
```Haskell
let myString = "Hello World"
let mySubstring = take 5 myString
```
This will give us the substring "Hello". 

In both examples, we used the `let` keyword to define a new variable for our substring. However, we can also use the `take` and `drop` functions directly within a larger expression.

## Deep Dive
Now let's take a deeper look at the `take` and `drop` functions. Both of these functions take in an integer value as their first argument, which determines the number of characters to take or drop from the string. 

The `take` function starts from the beginning of the string and takes the specified number of characters, while the `drop` function starts from the beginning of the string and drops the specified number of characters. 

It's important to note that both functions will return a new string, as Haskell strings are immutable.

## See Also
- [Haskell documentation on String functions](https://www.haskell.org/hoogle/?q=string#t:%5BChar%5D%20-%3E%20%5B%Char%5D)
- [How To Extract Substrings in Python](https://realpython.com/lessons/extracting-substrings-python/)
- [More on String Functions in Haskell](https://wiki.haskell.org/Character_string_functions)