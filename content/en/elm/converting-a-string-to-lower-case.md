---
title:                "Elm recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in programming, especially when dealing with user input. It ensures consistency and simplifies comparisons between strings.

## How To

Converting a string to lower case in Elm is a straightforward process. First, we need to import the `String` module:

```elm
import String exposing (toLower)
```

Next, we can use the `toLower` function to convert a string to lower case. Let's see an example:

```elm
toLower "Hello World" -- Output: "hello world"
```

We can also use the `toLower` function on a list of characters, which can be useful when dealing with user input:

```elm
toLower ['E', 'L', 'M'] -- Output: "elm"
```

As you can see, the function returns a string in all lowercase letters.

## Deep Dive

Behind the scenes, the `toLower` function iterates through each character in the string and uses the Unicode lowercasing algorithm to convert it to lower case. This ensures that all characters, including special characters from different languages, are correctly converted.

One important thing to note is that the `toLower` function does not modify the original string, but instead returns a new string in lowercase. This is because strings in Elm are immutable, meaning they cannot be changed after they are created.

Another thing to consider is that the `toLower` function only works on ASCII characters. If you need to convert non-ASCII characters to lower case, you can use the `map` function to apply the `toLower` function to each character in the string.

## See Also

- [String.lowercase function - Elm documentation](https://package.elm-lang.org/packages/elm-lang/core/latest/String#lowercase)
- [Unicode Lowercasing Algorithm](https://unicode.org/versions/Unicode13.0.0/ch03.pdf#page=144)