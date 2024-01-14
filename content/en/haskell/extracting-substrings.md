---
title:                "Haskell recipe: Extracting substrings"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a common task in programming, especially when working with text data. It involves taking a part of a larger string and using it separately. This can be useful when manipulating data, searching for specific patterns, or just organizing information.

## How To

### Basic Substring Extraction

In Haskell, substring extraction can be done using the `take`, `drop`, and `splitAt` functions. The `take` function takes the first n characters from a string, while the `drop` function drops the first n characters. The `splitAt` function takes a number n and a string, and returns a tuple with the substring up to n characters and the remaining characters.

```Haskell
-- Basic substring extraction using take, drop, and splitAt

-- take first 5 characters
take 5 "Hello, World!" 
-- output: "Hello"

-- drop first 5 characters
drop 5 "Hello, World!"
-- output: ", World!"

-- split at index 5
splitAt 5 "Hello, World!"
-- output: ("Hello", ", World!")
```

### Regex Substring Extraction

Haskell also has a `substring` function in the `Text.Regex.PCRE` module that allows for regular expression substring extraction. Regular expressions are useful for more complex patterns that cannot be easily handled with the basic substring functions.

```Haskell
-- Substring extraction using regular expressions

-- extract numbers from a string
substring "[0-9]+" "21 days left"
-- output: "21"

-- extract email domain
substring "@[a-z]+\\.[a-z]+" "email@example.com"
-- output: "@example.com"
```

### Partial Application

A useful technique when working with substring extraction is partial application. Partial application means applying a function to some of its arguments and returning a new function with the remaining arguments as parameters. This allows for greater flexibility when dealing with strings of different lengths.

```Haskell
-- Partial application for substring extraction using take

-- create a function to take the first n characters
substringN n = take n "Hello, World!"

-- take first 5 characters
substring5 = substringN 5 "Hello, World!"
-- output: "Hello"

-- take first 10 characters
substring10 = substringN 10 "Hello, World!"
-- output: "Hello, Worl"
```

## Deep Dive

There are many other functions and techniques that can be used for substring extraction in Haskell, such as `substr`, `breakOn`, `takeWhile`, and more. It is also important to consider the efficiency of substring extraction, as certain functions may be more efficient for large strings. Understanding the different options and techniques for substring extraction can greatly improve the efficiency and functionality of your code.

## See Also

- [Haskell `take` function](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:take)
- [Haskell `drop` function](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:drop)
- [Haskell `splitAt` function](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:splitAt)
- [Haskell `substring` function](http://hackage.haskell.org/package/regex-pcre-1.0.2/docs/Text-Regex-PCRE.html#v:substring)