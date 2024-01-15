---
title:                "Using regular expressions"
html_title:           "Haskell recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

If you're a developer or someone who deals with processing text and patterns, you've most likely heard of regular expressions. These powerful tools allow you to quickly search, extract, and manipulate text using a compact and concise syntax.

## How To

To use regular expressions in Haskell, we first need to import the `Text.Regex.Posix` module. Then, we can start using the functions within it, such as `match` and `subRegex`, to perform various operations on strings. Here's an example of how we can match a specific pattern and extract the matched text:

```Haskell
import Text.Regex.Posix

text = "Hello, my name is John. Nice to meet you."

match "Hello, my name is (.+)." text :: (String, String, String)
-- ("Hello, my name is John.", " John.", "John")
```

In the above code, we use the `match` function to find all strings that match the given pattern, `"Hello, my name is (.+)."`. By including `(.+)` within parentheses, we are indicating that we want to extract the text that matches this part of the pattern. The `:: (String, String, String)` at the end is specifying the types of the returned values, in order of the match itself, the remaining string after the match, and the extracted text.

Another useful function is `subRegex`, which allows us to replace matched patterns with a specified replacement text. Here's an example of how we can replace all instances of "John" with "Jane":

```Haskell
text = "Hello, my name is John. Nice to meet you."

subRegex (makeRegex "John") text "Jane" :: String
-- "Hello, my name is Jane. Nice to meet you."
```

In this example, we use the `makeRegex` function to create a regular expression from the string "John". Then, we use `subRegex` to replace all occurrences of "John" with "Jane" in the original text.

## Deep Dive

Regular expressions in Haskell are based on POSIX regular expressions, which means they follow the same syntax and conventions as other programming languages. This also means that you can use your existing knowledge of regular expressions in other languages to work with them in Haskell.

However, one unique feature of regular expressions in Haskell is the use of the `=~` operator. This allows you to easily match a string against a regular expression and return a `Bool` value indicating if there was a match or not. Here's an example:

```Haskell
text = "12345"

text =~ "[0-9]+" :: Bool
-- True
```

In this example, we use the `=~` operator to match the string "12345" against the regular expression "[0-9]+", which matches any string containing one or more digits. Since the input string contains only digits, the overall result is `True`.

## See Also

- [Haskell.org](https://www.haskell.org/)
- [Regex tutorial on Wikibooks](https://en.wikibooks.org/wiki/Regular_Expressions/POSIX_Basic_Regular_Expressions)
- [Official documentation for Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html)