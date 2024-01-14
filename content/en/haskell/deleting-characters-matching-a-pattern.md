---
title:                "Haskell recipe: Deleting characters matching a pattern"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Sometimes when writing code, we may encounter situations where we need to delete certain characters from a string that match a specific pattern. This could be to clean up user input, remove unnecessary characters, or simply to reformat the string for better processing. In such cases, knowing how to delete characters matching a pattern in Haskell can come in handy.

## How To

To delete characters matching a pattern in Haskell, we can use the `filter` function along with a predicate function that checks for the pattern. Here's an example of a function that removes all the vowels from a given string:

```Haskell
removeVowels :: String -> String
removeVowels str = filter (\c -> not (c `elem` "aeiouAEIOU")) str
```

In the above function, we use the `filter` function to check each character in the string against a predicate function that checks if the character is a vowel or not. If it's not a vowel, the character is included in the output string, effectively removing all the vowels.

Let's see this in action:

```Haskell
removeVowels "Hello, world!" -- Output: "Hll, wrld!"
```

We can also use more complex pattern matching expressions in the predicate function to remove specific characters or sequences. For example, let's say we want to remove all the special characters from a string, leaving only alphabets and numbers. We can use the `isAlphaNum` function from the `Data.Char` module along with `filter`:

```Haskell
import Data.Char (isAlphaNum)

removeSpecial :: String -> String
removeSpecial str = filter isAlphaNum str
```
Here, we use the `isAlphaNum` function as a predicate to check if each character is an alphabet or a number. Any character that doesn't match this pattern is filtered out, leaving us with only alphabets and numbers in the output string.

```Haskell
removeSpecial "Hello, world!" -- Output: "Helloworld"
```

## Deep Dive

The `filter` function works by taking a predicate function and a list, and returning a list with only those elements that satisfy the predicate. In our examples, we used `filter` with the `String` type, but it can be used with any list-like data structure in Haskell, such as `Data.IntMap`.

Additionally, we can also use the `delete` function from the `Data.List` module to specifically delete a single element from a list by comparing it with a given value. This can be useful if we know exactly which character(s) we want to delete.

## See Also

- [Haskell filter function documentation](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#x16-1780009)
- [Data.Char module documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Data.List module documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)