---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lowercase means transforming all of the uppercase letters in a string to their lowercase counterparts. We do this primarily for user input normalization, ensuring reliable comparison and search operations.

## How to:

Elm provides a `toLower` function within the `String` module to convert an input string to lowercase. Here's a simple example:

```Elm
import String

lowercaseString : String -> String
lowercaseString str = 
    String.toLower str

main = 
    print (lowercaseString "Hello ELM World!")
```

When you run this code, it outputs:

```
"hello elm world!"
```

## Deep Dive:

Historically, software systems didn't treat uppercase and lowercase letters as equal. This creates issues in string comparison and search operations. So, we use string normalization techniques like converting strings to lower case.

Alternative approaches could involve setting up custom comparison logic that's case-insensitive. But this approach isn't efficient, as it requires additional code and processing time.

In Elm, the `toLower` function uses Unicode's lower-mapping of characters. This means it not only handles ASCII characters but also works with non-English characters. It works by mapping each Unicode character in the string to its lower-case equivalent, resulting in a new string. Extra points for Elm, as the implementation is not language-specific.

## See Also:

Go deeper into strings in Elm following these links:

- Elm's [String library](https://package.elm-lang.org/packages/elm/core/latest/String) documentation