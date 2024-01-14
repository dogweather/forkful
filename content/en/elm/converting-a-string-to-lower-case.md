---
title:    "Elm recipe: Converting a string to lower case"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Why

Converting a string to lower case may seem like a small and inconsequential task, but in fact it can greatly improve the user experience of your software. By converting user input to lower case, you can ensure that your program is more user-friendly and easier to navigate.

# How To

```Elm
-- Let's take a look at a simple function that converts a string to lower case

import String

convertToLower : String -> String
convertToLower input =
    String.toLower input

-- Calling this function with any string input will result in a lower case output
convertToLower "ELM PROGRAMMING" -- "elm programming"

```

# Deep Dive

Now that we have a basic understanding of how to convert a string to lower case in Elm, let's take a deeper look. The String library in Elm provides us with a variety of functions to manipulate strings, including `toLower`, which performs exactly what we need.

It's worth noting that the `toLower` function uses the Unicode standard for lower case mapping, meaning it can handle special characters from different languages. This is especially useful if your program requires internationalization.

Another important aspect to keep in mind is that Elm, being a functional programming language, is immutable. This means that the original string input remains unchanged after using the `toLower` function, and a new output string is returned instead.

# See Also

- Elm String library documentation: https://package.elm-lang.org/packages/elm-lang/core/1.0.5/String
- Unicode lowercase conversions: https://unicode.org/charts/case/
- Official Elm guide on strings: https://guide.elm-lang.org/strings/