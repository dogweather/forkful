---
date: 2024-01-20 17:35:03.400584-07:00
description: 'How to: Haskell makes string concatenation pretty straightforward with
  the `(++)` operator.'
lastmod: '2024-03-13T22:45:00.120755-06:00'
model: gpt-4-1106-preview
summary: Haskell makes string concatenation pretty straightforward with the `(++)`
  operator.
title: Concatenating strings
weight: 3
---

## How to:
Haskell makes string concatenation pretty straightforward with the `(++)` operator:

```Haskell
main :: IO ()
main = do
  let hello = "Hello"
  let world = "World!"
  
  -- Using the (++) operator
  putStrLn $ hello ++ " " ++ world
  
  -- Sample output: "Hello World!"
```

But why stop there? You've also got `concat` and `intercalate` from `Data.List` for when lists get involved:

```Haskell
import Data.List (intercalate, concat)

main :: IO ()
main = do
  let wordsList = ["Haskell", "is", "cool"]
  
  -- Concatenating a list of strings
  putStrLn $ concat wordsList
  -- Sample output: "Haskelliscool"

  -- Interpolating strings with a separator
  putStrLn $ intercalate " " wordsList
  -- Sample output: "Haskell is cool"
```

## Deep Dive
Back in the day, Haskell's `++` operator took inspiration from similar operations in languages like ML. It's a classic, but not always the most efficient, especially for large strings or massive concatenation tasks. Each use of `++` creates a new list, meaning if you're working with big data, you might need a more efficient approach.

Alternatives? Absolutely. The `Builder` type from `Data.Text.Lazy.Builder` can be better optimized for large text manipulations. It constructs text more economically by working in chunks, reducing the need to constantly copy the whole enchilada.

For example, working with the `Builder`:

```Haskell
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Data.Text.Lazy.IO as T

main :: IO ()
main = do
  let builder1 = fromString "Haskell"
  let builder2 = fromString " "
  let builder3 = fromString "is"
  let builder4 = fromString " "
  let builder5 = fromString "neat!"

  let result = mconcat [builder1, builder2, builder3, builder4, builder5]
  -- Using mconcat to merge Builders

  T.putStrLn $ toLazyText result
  -- Sample output: "Haskell is neat!"
```

Why reach for `Builder` or `concat`? They handle big data without batting an eye, letting you combine text without drowning in performance issues.

## See Also
- The Haskell Wiki on [Performance/Strings](https://wiki.haskell.org/Performance/Strings) to dive deeper into performance considerations.
- The `Data.Text` [package documentation](https://hackage.haskell.org/package/text) for working with Unicode text in Haskell.
- The [Haskell Language website](https://www.haskell.org/) to keep up-to-date with all things Haskell.
