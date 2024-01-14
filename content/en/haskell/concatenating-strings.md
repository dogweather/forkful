---
title:    "Haskell recipe: Concatenating strings"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Why
Concatenating strings is a powerful tool in any programming language, and Haskell is no exception. By joining two or more strings together, we can create longer and more complex strings that can be used in a variety of ways. Whether you need to generate dynamic text, format data, or build URLs, understanding how to concatenate strings is an important skill for any Haskell programmer.

# How To
To concatenate strings in Haskell, we use the `++` operator. Let's take a look at an example:

```Haskell
str1 = "Hello"
str2 = "World"
concatStr = str1 ++ str2

print concatStr
```

In this code, we have two strings - "Hello" and "World" - and we use the `++` operator to combine them into one string. When we print out the `concatStr` variable, we get the output: "HelloWorld". We can also concatenate multiple strings at once by using the `++` operator in between each string.

```Haskell
str1 = "I"
str2 = "love"
str3 = "coding"
concatStr = str1 ++ str2 ++ str3

print concatStr
```

This time, the output would be: "Ilovecoding".

We can also use the `++` operator with string literals, variables, and even functions that return strings. The possibilities are endless!

# Deep Dive
Under the surface, Haskell uses the `append` function to concatenate strings. This function takes in two strings and returns a new string that is the result of concatenating them. Thus, when we use the `++` operator, we are essentially using the `append` function behind the scenes.

It's important to note that concatenation in Haskell is not just limited to strings. We can also use it with other data types such as lists and numbers. However, when concatenating numbers, we need to first convert them into strings using the `show` function.

# See Also
- [Haskell Official Documentation](https://www.haskell.org/documentation/)
- [Concatenation in Haskell](https://wiki.haskell.org/Concatenation)
- [Haskell String Tutorial](https://www.tutorialspoint.com/haskell/haskell_strings.htm)