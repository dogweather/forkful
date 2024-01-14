---
title:    "Haskell recipe: Finding the length of a string"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Haskell is a unique and increasingly popular functional programming language, known for its powerful type system and elegant solutions to complex problems. One common task in programming is finding the length of a string, and in this blog post, we will explore how this can be done in Haskell.

## How To

In Haskell, strings are represented as lists of characters. Therefore, finding the length of a string is equivalent to finding the length of a list. There are multiple ways to achieve this, and we will discuss two of them here.

The first method is to use the built-in function `length`, which takes a list as its input and outputs its length as an integer. Let's see an example:

```Haskell
length "Hello, world!" 
-- Output: 13
```

As you can see, this method is quite straightforward and requires minimal code. However, it is worth noting that the `length` function has to traverse the entire list, making it less efficient for longer strings.

Another method is to use recursion. We can define a function that takes a string as input and recursively counts the number of characters until the end of the string is reached. Here's an example:

```Haskell
strLength :: [Char] -> Int
strLength [] = 0 -- base case
strLength (_:xs) = 1 + strLength xs
```

In this function, we first check if the string is empty, in which case the length is 0. Otherwise, we add 1 to the length and recursively call the function on the remaining part of the string.

## Deep Dive

In Haskell, strings are represented as lists of characters due to the language's focus on immutability and purity. This means that strings are treated as first-class citizens, just like any other data type.

When using the `length` function, it is important to remember that it evaluates to an integer, which is a different type from a string. This is an example of Haskell's strong, static type system, which ensures type safety and prevents errors at runtime.

Furthermore, the recursive method we discussed earlier is a great example of how functional programming empowers developers to break down a problem into smaller, simpler components and solve them recursively. This is a fundamental concept to understand in Haskell and other functional languages.

## See Also

Here are some helpful links to further explore finding the length of a string in Haskell:

- [Haskell documentation on lists](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)
- [Haskell tutorial on recursion](https://guide.aelve.com/haskell/recursion-in-the-list-function-length-532)
- [Haskell function composition](https://thepad.github.io/articles/haskell-function-composition)