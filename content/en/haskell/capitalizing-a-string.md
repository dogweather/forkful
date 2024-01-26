---
title:                "Capitalizing a string"
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means fixing its casing so the first letter is uppercase, and the rest are lowercase. Programmers do this for consistency, readability, and to meet data formatting standards.

## How to:

To capitalize strings in Haskell, the language itself doesn't have a built-in `capitalize` function. So, we'll make our own using the `toUpper` and `toLower` functions from the `Data.Char` module.

```Haskell
import Data.Char (toUpper, toLower)

-- Capitalizes the first character of a string and lowercases the rest
capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : map toLower xs

main = do
  print $ capitalize "haskell"       -- Outputs "Haskell"
  print $ capitalize "hASKELL"       -- Outputs "Haskell"
  print $ capitalize ""              -- Outputs ""
  print $ capitalize "hello world!"  -- Outputs "Hello world!"
```

## Deep Dive

Haskell, a functional programming language, doesn't include simple string capitalization in its standard library, possibly because it's trivial to implement and not a common necessity in the type of programming it's designed for.

Alternatives to the `capitalize` function could use `Data.Text` which may provide performance benefits for large texts due to more efficient internal representations. Or look into libraries like `text-icu` for robust locale-sensitive capitalization.

Implementation wise, it's worth noting that our `capitalize` function doesn't deal with non-ASCII characters. If you need full Unicode support, you'd have to look for a library solution or handle complex cases of Unicode capitalization where simple character-by-character transformations don't cut it.

## See Also

- Haskell's `Data.Char` module: http://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html
- `Data.Text` for efficient text manipulation: http://hackage.haskell.org/package/text
- Introduction to text processing in Haskell: https://wiki.haskell.org/Text_Processing
- Unicode considerations in Haskell: https://wiki.haskell.org/Unicode_input_and_output
