---
date: 2024-01-20 17:57:57.492309-07:00
description: Searching and replacing text lets you find strings and swap them out.
  Programmers use it to update code, refactor, or change data quickly.
lastmod: '2024-02-25T18:49:56.557919-07:00'
model: gpt-4-1106-preview
summary: Searching and replacing text lets you find strings and swap them out. Programmers
  use it to update code, refactor, or change data quickly.
title: Searching and replacing text
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text lets you find strings and swap them out. Programmers use it to update code, refactor, or change data quickly.

## How to:

Let's search and replace text using Haskell. We'll use `Data.Text` for Unicode text handling and efficiency. Make sure to import `Data.Text` like this: 

```haskell
import qualified Data.Text as T
```

Now, let's replace all instances of "hello" with "hi" in a text:

```haskell
replaceText :: T.Text -> T.Text -> T.Text -> T.Text
replaceText old new = T.replace old new

main :: IO ()
main = do
  let originalText = T.pack "hello world, hello Haskell!"
  let newText = replaceText (T.pack "hello") (T.pack "hi") originalText
  print newText -- "hi world, hi Haskell!"
```

The `replace` function does the heavy lifting. We've wrapped it in `replaceText` for clarity.

## Deep Dive

Haskell's text replacement functions like `T.replace` are built on top of array processing capabilities of Haskell. Looking back, Haskell was first conceived in the '80s, with a focus on functional programming. This paradigm makes operations like text replacement elegant and less error-prone due to immutability and strong type systems.

As for alternatives, you could manually iterate over text and replace substrings, but that's more error-prone and inefficient. 

The `Data.Text` library uses a different internal representation than the `String` type (which is just a list of characters), making it better suited for large-scale text operations. The `T.replace` function itself uses efficient algorithms for string searching, which offer good performance even for large texts.

## See Also

For more on `Data.Text`, check out: 

- [Text package on Hackage](https://hackage.haskell.org/package/text)

Also consider broader reading on Haskell's string manipulation:

- [Haskell Wiki on strings](https://wiki.haskell.org/Strings)
- [Learn You a Haskell for Great Good! on Text](http://learnyouahaskell.com/input-and-output#files-and-streams)
