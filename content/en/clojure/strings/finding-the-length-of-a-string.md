---
date: 2024-01-20 17:47:16.973667-07:00
description: 'How to: To get a string''s length in Clojure, use the `count` function.'
lastmod: '2024-03-13T22:44:59.737267-06:00'
model: gpt-4-1106-preview
summary: To get a string's length in Clojure, use the `count` function.
title: Finding the length of a string
weight: 7
---

## How to:
To get a string's length in Clojure, use the `count` function:

```clojure
(count "Hello, World!") ;=> 13
```

This means "Hello, World!" has 13 characters.

## Deep Dive
The `count` function is the go-to in Clojure for finding the number of items in a collection, and strings are no exception since they can be treated as a sequence of characters. Historically, `count` has been part of Clojure since its early versions, reflecting its roots in Lisp where length operations are common on lists.

An alternative to `count` could be using Java interop because Clojure runs on the JVM:

```clojure
(.length "Hello, World!") ;=> 13
```

This calls the `.length` method from Java's String class. Though this alternative exists, using `count` is more idiomatic Clojure.

It's worth mentioning that `count` is O(1) operation for strings, which means it takes a constant amount of time regardless of string length since the string's length metadata is cached.

## See Also
- Clojure official docs on `count`: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count)
