---
title:                "Deleting characters matching a pattern"
html_title:           "Elm recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern is a commonly used technique in programming that involves removing certain characters from a given string based on a specific pattern or rule. Programmers use this method to manipulate strings and extract the desired information, making their code more efficient and concise.

## How to:

To delete characters matching a pattern in Elm, we can use the `filter` function from the `String` module. This function takes in two parameters: a boolean function that specifies the condition for filtering and a string to be filtered. Here's an example:

```Elm
import String exposing (filter)

myString = "Hello World!"

filteredString = filter (\c -> c /= 'o') myString

-- filteredString = "Hell Wrld!"
```

In this example, we use the `filter` function to remove all occurrences of the character 'o' from the string "Hello World!". We first define a boolean function that returns `True` if the given character is not equal to 'o', which is then used as the first parameter for `filter`. The result is a filtered string with all instances of 'o' removed.

## Deep Dive:

- **Historical Context:** The concept of deleting characters from a string has been around since the early days of programming. In fact, the first high-level programming language, Fortran, had a built-in function for deleting characters from a string. The technique has since been used in various programming languages, including Elm, to manipulate and extract information from strings.

- **Alternatives:** In Elm, there are a few other methods for deleting characters from a string, such as using regular expressions or the `String.replace` function. However, these methods may not be as efficient or concise as using `filter` in certain situations.

- **Implementation Details:** The `filter` function works by looping through the characters in a string and applying the given boolean function to each character. If the function returns `True`, the character is kept in the final string. However, if the function returns `False`, the character is skipped and not added to the final string. This process continues until all characters in the string have been checked.

## See Also:

- [Elm documentation on `String` module](https://package.elm-lang.org/packages/elm/core/latest/String)
- [W3Schools tutorial on filtering arrays](https://www.w3schools.com/jsref/jsref_filter.asp)
- [MDN Web Docs on regular expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)