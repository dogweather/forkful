---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Deleting Characters Matching a Pattern in Haskell

## What & Why?

Deleting characters matching a pattern is a method to cleanse the data or modify text. Whether it's for preprocessing, removing noise data, or ensuring data privacy, programmers often need to delete specific characters from strings.

## How to:

Here's an example, deleting all vowels in a string:

```Haskell
import Data.Char
import Data.List

deleteVowels :: String -> String
deleteVowels = filter (`notElem` ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'])

main :: IO ()
main = do
    let str = "Hello, World!"
    putStrLn $ deleteVowels str
```

When run, this spits out:

```Haskell
"Hll, Wrld!"
```

## Deep Dive

The idea of pattern matching is rooted in the symbolic manipulation of the Lisp language family. Haskell's implementation, however, is strongly influenced by the pattern matching of the ML language family. The `filter` function is the basic building block to accomplish our goal. It's one of the numerous high-order functions has Haskell offers.

For more complex patterns, you could use regular expressions, but the use of `filter` and `notElem` illustrates a powerful, idiomatic Haskell technique. If performance is a concern, consider using `Data.Text` instead of `String`, as the former is more optimized.

## See Also

For further reading:

- [Learn You a Haskell for Great Good](http://learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/)
- [Haskell Wiki](https://wiki.haskell.org/Main_Page) for in-depth developer resources.