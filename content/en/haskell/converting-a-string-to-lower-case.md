---
title:                "Converting a string to lower case"
html_title:           "Haskell recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to convert a string to lower case in Haskell but didn't know how? Well, look no further! In this article, we will explore the various ways to convert a string to lower case in Haskell.

## How To

In Haskell, there are multiple ways to convert a string to lower case. Let's take a look at two methods using the `map` function and the `toLower` function from the `Data.Char` module.

```
-- Using `map` function
map toLower "Hello World" -- Output: "hello world"

-- Using `toLower` function
import Data.Char
toLower 'A' -- Output: 'a'
```

As shown in the code examples, using the `map` function on a string will convert all the characters to lower case. Whereas, using the `toLower` function on a single character will convert that specific character to lower case. Both methods return the converted string or character as a list, so you may need to use the `concat` function to combine them into a single string.

## Deep Dive

To fully understand how converting a string to lower case works in Haskell, we must take a deeper dive into the `map` function. The `map` function takes a function as its first argument and a list as its second argument. It then applies the given function to each element in the list and returns a new list with the modified elements. In our case, the `toLower` function is the function being applied to each character in the string.

It is worth noting that the `map` function only works on lists, so if you need to convert a single character to lower case, you can use the `toLower` function directly. Additionally, the `toLower` function only works on characters, so if you need to convert a string with multiple words, you will need to use the `words` function to split the string into a list of words first and then use the `map` function on that list.

## See Also

- [Haskell Documentation on `map`](https://www.haskell.org/onlinereport/standard-prelude.html#map)
- [Haskell Documentation on `toLower`](https://hackage.haskell.org/package/base/docs/Data-Char.html#v:toLower)