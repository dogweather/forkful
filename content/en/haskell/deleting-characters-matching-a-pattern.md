---
title:                "Deleting characters matching a pattern"
html_title:           "Haskell recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters that match a particular pattern is a common task for programmers. It involves searching through a given string or text and removing any characters that match a specified criteria. This can be useful in data cleaning and formatting, as well as in filtering out unwanted data.

## How to:

To delete characters matching a pattern in Haskell, we can use the `filter` function. This function takes two arguments - a predicate function and a list. It returns a new list with only the elements that satisfy the given predicate.

Let's say we want to remove all vowels from a string. We can define a function that checks if a character is a vowel:

```Haskell
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"
```

Then, we can use `filter` to remove all vowels from a given string:

```Haskell
filter isVowel "hello world" -- Output: "hll wrld"
```

We can also use more complex patterns or regular expressions with the `filter` function to remove specific characters from a string.

```Haskell
import Text.Regex.Posix -- Import the regex library

-- Function that removes all digits from a string
removeDigits :: String -> String
removeDigits str = filter (\x -> not (x =~ "[0-9]" :: Bool)) str

removeDigits "ABC123" -- Output: "ABC"
```

## Deep Dive:

Pattern matching and filtering have been used in programming languages for a long time. In Haskell, the `filter` function is inspired by the mathematical concept of a filter. It takes a list and returns a new list with only the elements that match a certain criteria.

There are other ways of deleting characters in Haskell, such as using the `map` function or list comprehensions. These methods can also be used for deleting elements that match a pattern, but they may have different syntax and usage.

Under the hood, the `filter` function is implemented using recursion. When a condition is met for a particular element, it is added to the result list, and the function is recursively called on the rest of the elements. This continues until all elements have been checked.

## See Also:

- [Haskell filter function](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:filter)
- [Regular expressions in Haskell](https://hackage.haskell.org/package/regex-base-0.93.2/docs/Text-Regex-Base-RegexLike.html)