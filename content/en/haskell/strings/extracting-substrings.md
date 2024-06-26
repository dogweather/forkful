---
date: 2024-01-20 17:45:41.421904-07:00
description: 'How to: In Haskell, you can slice and dice strings with built-in functions
  like `take`, `drop`, and `substring` (from `Data.Text`).'
lastmod: '2024-03-13T22:45:00.118220-06:00'
model: gpt-4-1106-preview
summary: In Haskell, you can slice and dice strings with built-in functions like `take`,
  `drop`, and `substring` (from `Data.Text`).
title: Extracting substrings
weight: 6
---

## How to:
In Haskell, you can slice and dice strings with built-in functions like `take`, `drop`, and `substring` (from `Data.Text`).

```haskell
import Data.Text (Text, pack, unpack, take, drop)

-- Our example string
let exampleStr = "Haskell makes sense!"

-- Taking the first 7 characters
print $ unpack (take 7 (pack exampleStr)) -- "Haskell"

-- Dropping the first 8 characters
print $ unpack (drop 8 (pack exampleStr)) -- "makes sense!"

-- Custom function to extract a substring by position and length
substring :: Int -> Int -> Text -> Text
substring start length = take length . drop start

-- Extract "makes" (starting from position 8, length 5)
print $ unpack (substring 8 5 (pack exampleStr)) -- "makes"
```

Sample output:
```
"Haskell"
"makes sense!"
"makes"
```

## Deep Dive
Extracting substrings has been part of Haskell for ages. Early on, it relied on lists, since strings are lists of characters. Performance wasn't great. Enter `Data.Text`, offering efficient string operations.

Alternatives include list operations, regex, and parsing libraries. List ops are simpler but slower for big strings. Regex is powerful but overkill for simple tasks. Parsing libraries are for complex parsing but can handle substrings too.

Implementing a custom substring function in Haskell is straightforward using `take` and `drop` from `Data.Text`, providing faster string handling than list-based operations.

## See Also
- The `Data.Text` module documentation: https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html
- Learn You a Haskell for Great Good! for an easy dive into Haskell strings: http://learnyouahaskell.com/starting-out#immutability
- Real World Haskell for practical use cases: http://book.realworldhaskell.org/read/
- The Haskell Wiki for community insights: https://wiki.haskell.org/How_to_work_with_strings
