---
title:                "Searching and replacing text"
html_title:           "Elm recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is the process of searching for a specific word or phrase within a text and replacing it with another word or phrase. Programmers do this in order to quickly make changes to their code without having to manually go through each line. It saves time and ensures accuracy in making changes.

## How to:
Here's an example of how to search and replace text in Elm:

``` Elm
replaceText : String -> String -> String -> String
replaceText old new text =
    String.replace old new text
```

This code will take in three arguments: the old text you want to replace, the new text you want to replace it with, and the text you want to search through. It uses the built-in function `String.replace` to replace the old text with the new text in the given text. Here's an example of how to call this function:

``` Elm
replaceText "Hello" "Hi" "Hello, world!" -- returns "Hi, world!"
```

## Deep Dive:
Searching and replacing text has been a common feature in programming languages for a long time. It allows for quick and efficient editing of code, especially when making widespread changes. In Elm, the `String.replace` function is used to replace text. However, there are other ways to achieve the same result, such as using regular expressions or using the `replace` function in the `Dict` module.

## See Also:
- Official Elm documentation on `String.replace`: https://package.elm-lang.org/packages/elm-lang/core/latest/String#replace
- Elm package for regular expressions: https://package.elm-lang.org/packages/elm/regex/latest/
- `Dict` module documentation: https://package.elm-lang.org/packages/elm-lang/core/latest/Dict