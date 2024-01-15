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

## Why
Converting a string to lower case is a common task in programming that allows for easier manipulation and comparison of text. In Elm, this can be useful for creating text-based applications, performing text analysis, or simply ensuring consistency in user input.

## How To
To convert a string to lower case in Elm, we can use the `String.toLower` function. Here's an example of how it works:

```Elm
string = "ELM PROGRAMMING"
lowerString = String.toLower string
```

In this code, we first declare a string variable `string` and assign it the value of "ELM PROGRAMMING". Then, we use the `String.toLower` function to convert the string to lower case and assign it to a new variable `lowerString`. The output of `lowerString` would be "elm programming".

## Deep Dive
The `String.toLower` function utilizes the `String.map` function, which applies a function to each character in a string. In this case, the `Char.toLower` function is used to convert each character to its lower case equivalent. It also takes into consideration any unicode characters, making it a reliable and thorough method for converting strings to lower case.

Another useful function for manipulating strings is `String.toUpper`, which converts a string to all upper case. In addition, the `String.words` function can be used to split a string into a list of words, making it easier to perform operations on specific parts of the string.

## See Also
- [Elm Language Official Website](https://elm-lang.org/)
- [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm String Functions](https://guide.elm-lang.org/strings/index.html)