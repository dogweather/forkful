---
date: 2024-01-30 18:57:10.041442-07:00
description: 'How to: In Elm, you work with Dictionaries in the `Dict` module, so
  let''s dive into a quick example.'
lastmod: '2024-03-13T22:45:00.002922-06:00'
model: gpt-4-0125-preview
summary: In Elm, you work with Dictionaries in the `Dict` module, so let's dive into
  a quick example.
title: Using associative arrays
weight: 15
---

## How to:
In Elm, you work with Dictionaries in the `Dict` module, so let's dive into a quick example:

```Elm
import Dict exposing (Dict)

-- Initializing a dictionary with String keys and Int values
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- Adding or updating a value
updatedDict = Dict.insert "grape" 10 exampleDict

-- Retrieving a value (notice the Maybe type, as the key might not be present)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- Removing a key-value pair
finalDict = Dict.remove "banana" updatedDict

-- Converting a dictionary back to a list
dictToList = Dict.toList finalDict
```

Sample output when displaying `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

This demonstrates the basic operations: creating, updating, accessing, and iterating over a Dictionary.

## Deep Dive
Dictionaries in Elm internally use a structure known as an AVL tree - a type of self-balancing binary search tree. This choice strikes a balance between ensuring that operations like insert, get, and remove have good performance (logarithmic time complexity) and maintaining simplicity in handling the data.

Despite the strengths of Elm's `Dict`, it's not a one-size-fits-all solution. For collections that are ordered or need to be iterated over sequentially, List or Array might be more appropriate. Furthermore, when working with a fixed set of known keys, using custom types (Elm's version of enums) could offer more type safety and clearer intent in your code.

In the ecosystem of Elm, `Dict` offers a reliable way to manage collections of key-value pairs where the keys are unique and the order doesn't matter. While newer or more sophisticated structures may emerge, the `Dict` module remains a fundamental tool in the Elm programmer's toolkit for its simplicity and efficiency in handling associative arrays.
