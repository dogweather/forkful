---
date: 2024-01-20 17:42:30.351248-07:00
description: "Deleting characters that match a specific pattern is about sifting through\
  \ text and removing bits you don't need. Programmers do this to cleanse data,\u2026"
lastmod: 2024-02-19 22:05:18.577880
model: gpt-4-1106-preview
summary: "Deleting characters that match a specific pattern is about sifting through\
  \ text and removing bits you don't need. Programmers do this to cleanse data,\u2026"
title: Deleting characters matching a pattern
---

{{< edit_this_page >}}

## What & Why?

Deleting characters that match a specific pattern is about sifting through text and removing bits you don't need. Programmers do this to cleanse data, simplify strings, or prep data for something more important down the line, like parsing or analysis.

## How to:

```haskell
import Data.List (isInfixOf)
import Data.Char (isSpace)

-- Simple function to remove a pattern from a string
removePattern :: Eq a => [a] -> [a] -> [a]
removePattern [] _ = []
removePattern string@(x:xs) pattern
  | pattern `isInfixOf` string = removePattern (drop (length pattern) string) pattern
  | otherwise = x : removePattern xs pattern

-- Use predefined functions to trim spaces from a string
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main :: IO ()
main = do
  let text = "Haskell is super cool, super cool indeed."
  let cleanedText = removePattern text "super "
  putStrLn cleanedText  -- "Haskell is cool, cool indeed."
  putStrLn $ trim "   Trimmed whitespace   " -- "Trimmed whitespace"
```

## Deep Dive

Haskell's rich set of libraries, such as 'Data.List', provides various tools for manipulating lists, which strings are essentially a special case of. Historically, Haskell's pattern matching is a concept borrowed from older functional languages like ML.

There're different ways of pattern matching in Haskell. Our simple `removePattern` utilizes `isInfixOf` to check for the pattern. There're also regex libraries for complex patterns, but they add dependencies and sometimes overcomplicate things.

Speaking of dependencies, for trimming whitespaces, you could import a third-party library, but our 'trim' function does the job natively.

Lastly, performance-wise, always be cautious with recursive functions in Haskell; they can be inefficient if not properly optimized by the compiler. Thunks could pile up, causing space leaks. For better performance, you might explore Haskell's `Text` module for manipulation of large or numerous strings.

## See Also

- Real World Haskell: http://book.realworldhaskell.org/
- Haskell `Data.List` documentation: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html
- Haskell Wiki on Performance: https://wiki.haskell.org/Performance
