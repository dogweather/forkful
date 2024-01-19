---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# What & Why?

Searching and replacing text refers to locating specific strings within a larger text and substitifying them with a different string. Programmers perform this operation frequently to manipulate data, correct errors, or even refactor code.

# How to:

Here's how you can use the `String.replace` function in Elm to search and replace text.

```Elm
import String

main =
    let
        originalText = "I love apples"
        searchText = "apples"
        replaceText = "bananas"
        result = String.replace searchText replaceText originalText
    in
    Html.text result

-- Output will be "I love bananas"
```

In the code above, `originalText` is the string we are searching in. `searchText` is what we are looking for and `replaceText` is what we want to replace the `searchText` with. 

# Deep Dive

Searching and replacing text dates back to the early days of text editors. In Elm, `String.replace` is a simple and clean implementation of this functionality. It covers most use-cases. 

You might ask: Why not use regular expressions? The answer: Elm intentionally keeps it simple and does not (as of the current version) support regex to maintain simplicity and avoid intricate bugs.

If you need to perform a complex search and replace, you may need to write a custom function or use third-party libraries that do offer such functionality.

# See Also

- The Elm `String` library's official documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- For complex string operations, consider third-party libraries such as `elm/parser` (https://github.com/elm/parser).
- Interested in a wider discussion about the decision to exclude regex in Elm? Check out the discourse here: https://discourse.elm-lang.org/t/elm-and-regular-expressions/2036