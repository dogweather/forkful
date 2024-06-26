---
date: 2024-02-03 19:02:30.982258-07:00
description: 'How to: In Haskell, you can capitalize a string using the standard library
  without needing any third-party libraries.'
lastmod: '2024-03-13T22:45:00.112475-06:00'
model: gpt-4-0125-preview
summary: In Haskell, you can capitalize a string using the standard library without
  needing any third-party libraries.
title: Capitalizing a string
weight: 2
---

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
