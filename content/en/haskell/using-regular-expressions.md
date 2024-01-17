---
title:                "Using regular expressions"
html_title:           "Haskell recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions, or regex, is a sequence of characters used to define a search pattern. It allows programmers to efficiently search through text and extract specific information. It is commonly used to validate input in web forms, search through large datasets, and transform data. 

## How to:

Using regular expressions in Haskell is simple and powerful. The `Text.Regex.Posix` module provides functions for working with regex. 

To search for a pattern in a string, use the `=~` operator followed by the regex pattern and the string you want to search. For example, to find all email addresses in a string, you can do:

```Haskell
import Text.Regex.Posix ((=~))

let text = "My email is john@example.com"
let pattern = "[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}"
let matches = text =~ pattern :: Bool
```

The `=~` operator returns a `Bool` value, indicating whether the pattern was found in the string or not. 

To extract specific information from a string, use the `=~~` operator, followed by the string to match against and a `Capture` representing the capture group. For example, to extract the domain name from an email address, you can do:

```Haskell
import Text.Regex.Posix ((=~))

let email = "john@example.com"
let captureGroup = email =~ "[a-z0-9._%+-]+@([a-z0-9.-]+\\.[a-z]{2,4})"
let domain = captureGroup :: [String]
```

The `domain` variable will now hold the domain name from the email address. 

## Deep Dive:

Regular expressions have been around since the 1950s and have since been implemented in many programming languages, including Haskell. In addition to the `Text.Regex.Posix` module, Haskell also has the `Text.Regex.PCRE` module for working with Perl Compatible Regular Expressions (PCRE). 

Alternatives to using regular expressions in Haskell include string manipulation functions, such as `splitOn` and `intercalate` from the `Data.List.Split` module, or using the powerful parser combinator libraries like `parsec` and `attoparsec`. 

Regular expressions in Haskell are implemented using automata theory, specifically finite state machines. This allows for efficient matching of patterns, making it a popular choice for data transformation and validation tasks. 

## See Also:

- [HaskellRegex - Regex tutorial for beginners](https://wiki.haskell.org/HaskellRegex)
- [Real World Haskell - Chapter on regular expressions](http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html)
- [Hackage - Text.Regex package documentation](https://hackage.haskell.org/package/regex-base)