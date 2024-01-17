---
title:                "Extracting substrings"
html_title:           "Elm recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings is the process of breaking down a larger string of text into smaller pieces. This is a common task for programmers when they need to manipulate or analyze certain parts of a text string separately. For example, if you have a long email address and you only want to use the first part of it, you would need to extract that substring.

## How to:
Extracting substrings in Elm is simple and straightforward. There are built-in functions that allow us to easily manipulate strings. Let's take a look at some coding examples and their output:

```Elm
--Getting a substring from a specific index
String.slice 3 6 "Hello, World!" 
--Output: "lo,"

--Getting a substring from a specified start position and length
String.left 5 "Elm Programming"
--Output: "Elm P"

--Concatenating multiple substrings
String.concat ["Elm", "is", "fun"]
--Output: "Elmisfun"

--Replacing a part of a string with another string
String.replace "old" "new" "old phrase"
--Output: "new phrase"
```

## Deep Dive:
Extracting substrings has been a fundamental function in programming languages since the early days. It allows us to manipulate text strings and extract relevant information from them. While Elm has built-in functions for extracting substrings, there are also alternative approaches such as using regex or creating custom functions. Additionally, the implementation details of substring extraction in Elm are optimized for performance, making it a reliable and efficient option for developers.

## See Also:
- Official Elm String Library: https://package.elm-lang.org/packages/elm/core/latest/String 
- Regex in Elm: https://package.elm-lang.org/packages/elm/regex/latest/
- Custom functions in Elm: https://guide.elm-lang.org/effects/string.html