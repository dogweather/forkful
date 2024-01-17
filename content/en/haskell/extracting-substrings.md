---
title:                "Extracting substrings"
html_title:           "Haskell recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings is the act of taking a specific portion or sequence of characters from a larger string. This is a commonly used operation in programming because it allows us to easily manipulate and work with specific parts of a string.

## How to:
To extract substrings in Haskell, we can use the `take` and `drop` functions from the `Prelude` module. The `take` function takes a specified number of characters from the beginning of a string, while the `drop` function removes a specified number of characters from the beginning of a string. Let's take a look at an example:

```Haskell
str = "Hello world!"
take 5 str
-- Output: "Hello"

drop 6 str
-- Output: "world!"
```

Another useful function for extracting substrings is the `substring` function from the `Data.String.Utils` module. This function allows us to specify a starting index and length to extract a substring from a string. Here's an example:

```Haskell
import Data.String.Utils (substring)

str = "Hello world!"
substring 6 5 str
-- Output: "world"
```

## Deep Dive:
Extracting substrings has been a commonly used operation in programming since the early days. In fact, the `substring` function in Haskell was originally inspired by the `substr` function in the C programming language.

There are also other alternatives for extracting substrings in Haskell, such as using list comprehensions or regular expressions. However, the `substring` function is often preferred because of its simplicity and efficiency.

When it comes to the implementation, the `substring` function uses the `splitAt` function to split the original string at the specified starting index, and then uses the `take` function to extract the desired length.

## See Also:
- [Haskell documentation for `take` function](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:take)
- [Haskell documentation for `drop` function](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:drop)
- [Hackage page for `substring` function](https://hackage.haskell.org/package MissingH-1.4.0.1/docs/Data-String-Utils.html#v:substring)