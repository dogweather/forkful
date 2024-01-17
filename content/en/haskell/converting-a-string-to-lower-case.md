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

# Lowercasing Strings in Haskell

## What & Why?
Converting a string to lower case is simply the process of changing all uppercase letters in a given string to their corresponding lowercase letters. This is a common operation in programming, as it allows for an easier and more efficient way to compare, search, and manipulate strings.

## How to:
In Haskell, the `toLower` function from the `Data.Char` module can be used to convert a string to lower case. This function takes a single `Char` as input and returns the lowercase version of that character. To apply it to an entire string, we can use the `map` function, which applies a given function to each element in a list.

```Haskell
import Data.Char (toLower)

lowercase :: String -> String
lowercase str = map toLower str

main = do
  let str = "Hello, WORLD!"
  putStrLn (lowercase str)
```

Output:
```
hello, world!
```

## Deep Dive:
In Haskell, strings are represented as lists of characters, with type `String` being equivalent to `[Char]`. This means that the `lowercase` function above can also be written as a simple `map` operation on a list of characters. Another alternative is to use list comprehension, which provides a more readable and concise way to perform the conversion.

```Haskell
lowercase' :: String -> String
lowercase' str = [toLower ch | ch <- str]
```

Another important thing to note is that the `toLower` function only works on ASCII characters. For Unicode support, the `Data.Text` module provides a `toLower` function that works on any Unicode character.

## See Also:
- [Haskell String Library](https://hackage.haskell.org/package/base/docs/Data-String.html)
- [Haskell Unicode Support](https://www.haskell.org/tutorial/strings.html#unicode)