---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
String concatenation is the process of combining two or more strings into a single one. Programmers do this to manage and manipulate text data effectively. 

## How to:
In Haskell, there are several ways to concatenate strings. Let's dive in:

1. Using the `(++)` operator:

```Haskell
main = do
  let str1 = "Hello, "
  let str2 = "World!"
  let str3 = str1 ++ str2
  putStrLn str3
```

This will output: `Hello, World!`

2. Using the `concat` function:

```Haskell
main = do
  let strs = ["Hello, ", "World!"]
  let str = concat strs
  putStrLn str
```

This will also output: `Hello, World!`

## Deep Dive
1. *Historical context*: The Haskell `(++)` operator for string concatenation and the `concat` function both find roots in the functional programming paradigm where functions and operators are used to process data.

2. *Alternatives*: Apart from `(++)` and `concat`, you can use `mconcat` or the `<>` operator from the `Monoid` typeclass, especially when working with Text or ByteString which are commonly used for efficiency over Strings.

3. *Implementation details*: Under the hood, `(++)`, `concat`, `mconcat`, and `<>` create a new string that combines the original strings. Be mindful of this, as the time complexity of `(++)` is O(n), and this may affect performance with longer strings.

## See Also
You might find these resources helpful:

1. Hoogle - handy Haskell search engine. [Hoogle](https://www.haskell.org/hoogle)

2. Real World Haskell, Chapter 8: Efficient file processing, regular expressions, and file name matching. [Real World Haskell, Chapter 8](http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html)

3. The Haskell wikibook - it's comprehensive and open-source. [Haskell wikibook](https://en.wikibooks.org/wiki/Haskell)