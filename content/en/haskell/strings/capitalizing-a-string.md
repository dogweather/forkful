---
aliases:
- /en/haskell/capitalizing-a-string/
date: 2024-02-03 19:02:30.982258-07:00
description: "Capitalizing a string involves transforming the first letter of a given\
  \ string to uppercase while ensuring the rest of the letters remain lowercase.\u2026"
lastmod: 2024-02-18 23:09:11.079463
model: gpt-4-0125-preview
summary: "Capitalizing a string involves transforming the first letter of a given\
  \ string to uppercase while ensuring the rest of the letters remain lowercase.\u2026"
title: Capitalizing a string
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string involves transforming the first letter of a given string to uppercase while ensuring the rest of the letters remain lowercase. Programmers do this for formatting outputs, adhering to grammatical correctness in texts, or improving the readability of generated data.

## How to:
In Haskell, you can capitalize a string using the standard library without needing any third-party libraries.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- Sample usage:
main = putStrLn $ capitalize "hello world"
```

Output:
```
Hello world
```

For more complex scenarios or ease of use, you might want to use a third-party library such as `text`, which is popular for efficient string manipulation in Haskell.

First, you need to add `text` to your project's dependencies. Then, you can use its functions to capitalize a string as follows:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- Sample usage with the text library:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

Output:
```
Hello world
```

Both of these examples demonstrate simple yet effective ways to capitalize a string in Haskell, with or without third-party libraries.
