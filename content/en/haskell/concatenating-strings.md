---
title:                "Haskell recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
Concatenating strings is a fundamental operation in programming. It allows us to combine multiple pieces of text into one string, making it easier to manipulate and display information. This is especially useful when working with user input or generating dynamic output.

## How To
To concatenate strings in Haskell, we can use the `++` operator or the `concat` function. Let's take a look at some examples using these methods:

```Haskell
str1 = "Hello "
str2 = "world!"
str3 = str1 ++ str2
-- Output: "Hello world!"

str4 = concat ["Welcome ", "to ", "Haskell"]
-- Output: "Welcome to Haskell"
```

In the first example, we use the `++` operator to combine the strings `str1` and `str2`. This operator works by appending the second string to the end of the first string. In the second example, we use the `concat` function to combine a list of strings into one. This can be useful when we have more than two strings to concatenate.

We can also use the `++` operator and `concat` function on lists of characters. Let's see an example:

```Haskell
char1 = ['H', 'e', 'l', 'l', 'o']
char2 = ['w', 'o', 'r', 'l', 'd']
char3 = char1 ++ char2
-- Output: "Helloworld"

char4 = concat [char1, char2]
-- Output: "Helloworld"
```

As we can see, both methods work the same way with lists of characters. This means we can easily manipulate strings as lists of characters and then concatenate them back into strings.

## Deep Dive
Under the hood, the `++` operator and `concat` function in Haskell use the `semigroup` typeclass to combine strings. This typeclass defines the `<>` operator, which is used for combining two values of the same type together. For strings, the `semigroup` instance uses the `++` operator to combine two strings.

It's worth noting that in Haskell, strings are not treated as a special data type. They are simply a list of characters, which allows us to apply list operations to strings. This makes concatenation much more flexible and versatile compared to other programming languages.

## See Also
- Haskell String Documentation: https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-String.html
- Learn You a Haskell: http://learnyouahaskell.com/starting-out#strings-lists-and-tuples