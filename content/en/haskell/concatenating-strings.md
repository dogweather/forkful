---
title:                "Haskell recipe: Concatenating strings"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

String concatenation is a fundamental concept in programming where we combine two or more strings into one. This can be useful when we need to display information to users or manipulate strings for data processing. In Haskell, string concatenation is a powerful tool that allows us to create new strings from existing ones.

## How To

To concatenate strings in Haskell, we can use the `++` operator. Let’s take a look at an example:

```Haskell
let firstWord = "Hello"
let secondWord = "World"
print (firstWord ++ secondWord)
```

The output of this code will be: `HelloWorld`

We can also combine more than two strings by chaining multiple `++` operators:

```Haskell
let sentence = "The" ++ " quick" ++ " brown" ++ " fox"
print sentence
```

The output of this code will be: `The quick brown fox`

We can also use string interpolation to dynamically concatenate strings. For example:

```Haskell
let myName = "John"
let age = 30
let greeting = "Hello, my name is " ++ myName ++ " and I am " ++ show (age) ++ " years old!"
print greeting
```

The `show` function is used to convert the integer value of `age` into a printable string. The output of this code will be: `Hello, my name is John and I am 30 years old!`

## Deep Dive

In Haskell, strings are represented as lists of characters. Therefore, string concatenation is essentially an operation on lists. This makes it easy to use functions like `map` and `filter` to manipulate strings.

We can also use the `++` operator with empty strings to concatenate multiple strings together. For example:

```Haskell
let empty = ""
let helloWorld = "Hello" ++ empty ++ "World"
print helloWorld
```

The output of this code will be: `HelloWorld`

Another important thing to note is that the `++` operator has a higher precedence than other operators in Haskell. This means that it will be evaluated first when used in conjunction with other operators. Therefore, it is important to use parentheses when necessary to avoid unexpected results.

## See Also
- [Haskell Documentation on Strings](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-String.html)
- [Functional Programming in Haskell](https://wiki.haskell.org/Functional_programming)
- [Haskell for Beginners](https://www.haskell.org/learn/)