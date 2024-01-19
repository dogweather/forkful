---
title:                "Capitalizing a string"
html_title:           "Elm recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizing Strings in Elm

## What & Why?

“Capitalizing a string” means changing the first letter of each word in that string to uppercase. We do it to create a better user experience and meet style requirements, given that capitalized words often lead to improved text readability. 

## How to:

Elm's standard library doesn't directly provide a function to capitalize strings. No worries, with a little tinkering, you can get the job done with `String.toUpper` and `String.uncons`.

```elm
capitalize : String -> String
capitalize word =
    case String.uncons word of
        Nothing ->
            ""

        Just (firstChar, restOfString) ->
            String.toUpper (String.fromChar firstChar) ++ restOfString
```
Check out this sample output:
```elm
capitalize "elm" -- Outputs: "Elm"
```
## Deep Dive

Historically, Elm emphasizes simplicity and safety, so it offers built-in functions to convert all characters of a string to upper or lower case but doesn't provide a direct way to capitalize strings. However, Elm's `String.uncons` function, that breaks a string into head and tail, comes in handy here.

There are alternatives around. For example, you could write a function that uses `String.words` and `String.unwords` to capitalize every word in a string. Importantly, keep in mind that `capitalize` is not a universally-defined operation. It might treat special characters, numbers, or non-English letters in unexpected ways.

Implementation details are less about the function's inner workings, and more about understanding `String.toUpper` and `String.uncons`. `String.toUpper` converts entire strings to uppercase, while `String.uncons` separates the string into its first character and the rest, giving us the hook to manipulate each word's first letter.

## See Also

- [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Function Syntax](https://guide.elm-lang.org/types/function_types.html)
- [Understanding Strings in Elm](https://thoughtbot.com/blog/understanding-strings-in-elm)