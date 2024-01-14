---
title:    "Elm recipe: Extracting substrings"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

When working with strings in any programming language, it is often necessary to extract a specific portion of the string. This could be to extract a name from an email address, to retrieve a date from a larger string, or to simply break down a string into smaller, more manageable parts. In Elm, there are built-in functions that allow for easy extraction of substrings, making it a useful tool for any programmer.

## How To

In order to extract a substring in Elm, the `String` module offers two main functions: `slice` and `dropLeft`. The first function, `slice`, takes in a starting index and an ending index, and returns the substring between those two points. On the other hand, `dropLeft` takes an integer representing the number of characters you want to skip and returns the remaining substring.

```
Elm String.slice example:

import String

email = "johnsmith@example.com"

firstName = String.slice 0 4 email
-- Output: john

lastName = String.slice 5 (String.length email) email
-- Output: smith
```

```
Elm String.dropLeft example:

import String

sentence = "I love programming in Elm."

firstWord = String.dropLeft 2 sentence
-- Output: love programming in Elm.
```

As shown in these examples, the index used for slicing starts at 0 and is non-inclusive, meaning the character at the ending index will not be included in the substring.

## Deep Dive

While the two built-in functions provide enough functionality for most substring needs, it is important to note that they both have some limitations. The `slice` function only works with strings, meaning it cannot be used for extracting substrings from other types such as lists or tuples. Additionally, both `slice` and `dropLeft` do not handle negative indexes, so it is necessary to manipulate the input string if the desired starting point is at the end of the string.

## See Also

- Elm String Documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- Substring in JavaScript: https://www.w3schools.com/jsref/jsref_substring.asp
- Manipulate strings with String indices in Python: https://docs.python.org/3/library/stdtypes.html#string-methods