---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lowercase is a common task in text processing when we want to make a string case-insensitive for comparisons, searches, or other operations. It streamlines the user input and eliminates arbitrary differences.

## How to:

In Haskell, we use the `Data.Char` library, specifically the `toLower` function. Here's a sample:

```haskell
import Data.Char (toLower)

lowercase :: String -> String
lowercase = map toLower
```

Let's try it out:

```haskell
print (lowercase "HELLO HASKELL")
```

The output will be:

```
"hello haskell"
```

## Deep Dive

The concept of lowercasing isn't unique to Haskell; it exists in almost every language. Early programming languages like FORTRAN and COBOL recognized the need to manipulate text data efficiently. 

In Haskell, the simplicity of lowercasing a string lies in its functional programming paradigm. Each character in the string is an element in a list. Mapping the `toLower` function over the list modifies each character independent of others.

There are other ways to convert a string to lowercase. For instance, using list comprehension:

```haskell
import Data.Char (toLower)

lowercase :: String -> String
lowercase str = [toLower c | c <- str]
```

But mapping a function is more idiomatic and a bit faster due to built-in optimizations in Haskell's compiler (GHC).

## See Also

Take a look at these sources for further learning:

* [Haskell - Data.Char](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
* [Learn you a Haskell for great good - A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads)
* [Real world Haskell - Chapter on Text processing](http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html)