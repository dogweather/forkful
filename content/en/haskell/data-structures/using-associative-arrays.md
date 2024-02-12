---
title:                "Using associative arrays"
aliases:
- /en/haskell/using-associative-arrays.md
date:                  2024-01-30T18:57:14.735713-07:00
model:                 gpt-4-0125-preview
simple_title:         "Using associative arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/using-associative-arrays.md"
changelog:
  - 2024-01-30, dogweather, reviewed
---

{{< edit_this_page >}}

## What & Why?

Associative arrays, or dictionaries, in Haskell are all about mapping keys to values for quick lookup and efficient data management. Programmers use them to handle collections of paired elements, where searching for an element is a breeze, compared to lists.

## How to:

Haskell doesn't have associative arrays out of the box in the same way as some other languages, but it offers a powerful standard library called `Data.Map` for working with key-value pairs. Let's roll up our sleeves and see how to use them!

First, make sure to import it:
```Haskell
import qualified Data.Map as Map
```

Creating a map is straightforward. Let's create one with some programming languages and their paradigms:
```Haskell
let languages = Map.fromList [("Haskell", "Functional"), ("Python", "Imperative"), ("Prolog", "Logical")]
```

Now, how about getting the paradigm of Haskell?
```Haskell
Map.lookup "Haskell" languages
-- output: Just "Functional"
```

Adding a new language is easy:
```Haskell
let languagesUpdated = Map.insert "Rust" "Systems" languages
```

What if we want to list all languages? Use `Map.keys`:
```Haskell
Map.keys languagesUpdated
-- output: ["Haskell","Python","Prolog","Rust"]
```

To list the paradigms, use `Map.elems`:
```Haskell
Map.elems languagesUpdated
-- output: ["Functional","Imperative","Logical","Systems"]
```

These basic operations should cover most uses, but there's plenty more to explore in `Data.Map`!

## Deep Dive

The `Data.Map` module in Haskell's standard library is built atop balanced binary trees, specifically AVL trees. This choice ensures that most operations on the map, such as insertion, deletion, and lookup, can be done in O(log n) time, where n is the number of elements in the map. It's an efficient choice for many use cases, though not the absolute fastest for all scenarios.

There's a historical nuance as well: before `Data.Map` became the go-to, Haskell programmers often used lists of pairs to simulate associative arrays. However, operations on such structures are O(n) for lookup, making `Data.Map` a significant improvement in terms of performance.

Now, despite the efficiency and utility of `Data.Map`, it's not always the best tool for every job. For highly performance-sensitive tasks, where even O(log n) lookup times are too slow, or where keys are always integer values, arrays or hash tables (via `Data.HashMap`) might offer better performance with O(1) access times.

The Haskell ecosystem allows for a variety of data structures to suit different needs, and `Data.Map` is an excellent general-purpose choice for associative arrays, balancing ease of use, flexibility, and performance.
