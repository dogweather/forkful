---
title:                "Converting a string to lower case"
html_title:           "Elm recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lower case is a process of converting all the characters in a string to their lower case equivalents. This is done to ensure consistency and compatibility when working with strings in a programming language. It also makes comparison and manipulation of strings easier.

## How to:
Converting a string to lower case in Elm is simple and straightforward. It can be done using the `String.toLower` function. This function takes in a string as input and returns a new string with all the characters in lower case.

```Elm
myString = "HeLlo WORlD"
lowerCaseString = String.toLower myString
-- lowerCaseString = "hello world"
```

In the above example, we have a string `myString` which contains a mix of upper and lower case characters. By using the `String.toLower` function, we get a new string `lowerCaseString` with all the characters in lower case.

## Deep Dive:
Converting strings to lower case has been a basic function in programming languages since the early days. It was first introduced in the programming language BCPL in the 1960s. Today, all major programming languages have a similar function to convert strings to lower case.

There are alternative ways to convert strings to lower case in Elm, such as using regular expressions or manually iterating through each character and converting it to lower case. However, the `String.toLower` function is the most efficient and recommended way to do it.

The `String.toLower` function internally uses the Unicode standard to determine the lower case equivalent of each character. This ensures compatibility and accuracy when working with different languages and character sets.

## See Also:
- [String.toLower documentation](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Unicode standard](https://unicode.org/)