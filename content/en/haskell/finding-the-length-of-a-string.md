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

## Why

Finding the length of a string is a common problem in programming, regardless of the language being used. In Haskell, a functional and strongly typed language, it can be solved in an elegant and simple way.

## How To

To start, we need to understand how strings are represented in Haskell. Unlike other languages, Haskell does not have a built-in string datatype. Instead, strings are represented as lists of characters. This means that finding the length of a string is essentially the same as finding the length of a list.

To find the length of a string, we can use the `length` function. This function takes in a list as its argument and returns an integer representing the length of the list.

Let's take a look at an example:

```Haskell
length "Hello World"
```
Output:
`11`

In this example, we are passing the string "Hello World" as the argument to the `length` function and it returns the length of the string, which is 11.

We can also use the `length` function on other types of lists, not just strings. For instance, we can find the length of a list of numbers like this:

```Haskell
length [1, 2, 3, 4, 5]
```
Output:
`5`

We can even combine multiple lists and find the total length, like this:

```Haskell
length ([1, 2, 3, 4] ++ [5, 6, 7])
```
Output:
`7`

Now that we have a basic understanding of how the `length` function works, let's take a closer look at how it is implemented.

## Deep Dive

The `length` function is actually a higher-order function, meaning that it takes in a function as an argument or returns a function as its result. In this case, the `length` function takes in a list as its argument and returns an integer as its result.

Internally, `length` uses recursion to iterate through the list and count the number of elements. This means it breaks the problem down into smaller parts and combines the results to get the final answer. Let's take a look at a simplified version of the `length` function:

```Haskell
length' [] = 0
length' (x:xs) = 1 + length' xs
```

Here, we have defined a function called `length'` that takes in a list as its argument. The first line is called the base case, and it simply states that the length of an empty list is 0. The second line is the recursive case, where we take the head of the list `x` and add 1 to it, then call `length'` again on the rest of the list `xs`.

This process repeats until we reach the base case, at which point the function stops recursing and returns the final result.

## See Also

* [Haskell String Tutorial](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
* [Learn You a Haskell - String Theory](http://learnyouahaskell.com/starting-out#im-a-list-comprehension)
* [Hoogle - Searching Haskell Libraries](https://hoogle.haskell.org/)