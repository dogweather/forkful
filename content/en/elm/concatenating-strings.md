---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings means combining two or more strings together, end to end. Programmers do it to manipulate and construct complex data, such as messages, file contents, or even chunks of code.

## How to:

In Elm (0.19.1), we use the `(++)` operator to concatenate strings. Here's a simple example:

```Elm
name = "John"
greeting = "Hello, " ++ name ++ "!"
```

After running this, `greeting` would be `"Hello, John!"`.

## Deep Dive

String concatenation, in essence, has been in programming since the beginning. It's fundamental to how we assemble data in a readable, usable form. In Elm, `(++)` does the job, but remember, Elm's not an inherently mutating language. So `(++)` doesn't alter original strings, but results in a new string.

An alternative is using `String.concat`, especially when joining a list of strings. Here's an example:

```Elm
String.concat ["Hello", ",", " ", "world", "!"]
```

The good news about `(++)` and `String.concat` is that they're both efficient. Under the hood, they utilize persistent data structures to avoid unnecessarily duplicating strings, resulting in a more performant operation.

## See Also

- Elm Guide on Strings: https://guide.elm-lang.org/types/string.html
- Elm Core Library’s String Interface: https://package.elm-lang.org/packages/elm/core/latest/String
- A deeper discussion on String concatenation: https://elmprogramming.com/string-concatenation.html
- The trade-offs between `(++)` and `String.concat`: https://stackoverflow.com/questions/27408567/elm-string-concatenation-versus