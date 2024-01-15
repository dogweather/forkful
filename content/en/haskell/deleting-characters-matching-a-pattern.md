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

## Why

Often in programming, we encounter situations where we need to manipulate strings to achieve a certain result. Deleting characters matching a specific pattern can be useful when cleaning up user input or processing large amounts of data.

## How To

To delete characters matching a pattern in Haskell, we can use the `filter` function along with a custom predicate. Let's say we want to remove all numbers from a string, we could write a function like this:

```Haskell
removeNumbers :: String -> String
removeNumbers xs = filter (\x -> x `notElem` "0123456789") xs
```

In this example, we use `filter` to remove all characters that are not a number from the given string. The `notElem` function checks if a character is not present in a given list. We can then call this function with our desired string and the output will be the original string without any numbers.

```Haskell
removeNumbers "abc123xyz" -- Output: "abcxyz"
```

We can also use regular expressions to remove characters matching a specific pattern. The `subRegex` function from the `Text.Regex` module can be useful for this purpose. Here's an example of a function that removes all vowels from a string using regular expressions:

```Haskell
import Text.Regex (subRegex)

removeVowels :: String -> String
removeVowels xs = subRegex (mkRegex "[aeiou]") xs ""

-- `mkRegex` is used to create a regex pattern, and the third argument is the replacement string
```

```Haskell
removeVowels "hello world" -- Output: "hll wrld"
```

## Deep Dive

In Haskell, strings are represented as lists of characters. When we use the `filter` function, it traverses through the entire list, applies the given predicate to each element, and returns a new list with the filtered elements. This makes it an efficient way to delete characters matching a pattern.

We can also combine multiple predicates to create more complex filters. For example, if we want to remove all uppercase letters and numbers from a string, we can do so by using the `all` function along with `isUpper` and `isDigit` from the `Data.Char` module:

```Haskell
import Data.Char (isUpper, isDigit)

removeUpperAndDigits :: String -> String
removeUpperAndDigits xs = filter (\x -> all (not . ($ x)) [isUpper, isDigit]) xs
```

In the above code, we use the `all` function to check if all predicates (i.e. `isUpper` and `isDigit`) return false for a given character, and only then the character gets filtered out.

## See Also

- [Haskell String Manipulation - Real World Haskell](https://www.realworldhaskell.org/v2/io-and-command-line-arguments.html#more-io-stringmanipulation)
- [Haskell String Functions - Hoogle](https://hoogle.haskell.org/?hoogle=String+-%3E+String)
- [Working with lists - Learn You a Haskell](http://learnyouahaskell.com/starting-out#ready-set-go)