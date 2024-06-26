---
date: 2024-01-20 17:41:52.447367-07:00
description: "How to: To remove characters using a pattern in Clojure, you employ\
  \ regular expressions with the `re-seq`, `re-find`, or `re-matches` functions paired\u2026"
lastmod: '2024-03-13T22:44:59.731091-06:00'
model: gpt-4-1106-preview
summary: To remove characters using a pattern in Clojure, you employ regular expressions
  with the `re-seq`, `re-find`, or `re-matches` functions paired with `clojure.string/replace`.
title: Deleting characters matching a pattern
weight: 5
---

## How to:
To remove characters using a pattern in Clojure, you employ regular expressions with the `re-seq`, `re-find`, or `re-matches` functions paired with `clojure.string/replace`.

```Clojure
(require '[clojure.string :as str])

;; Remove all digits from a string
(str/replace "He110 W0rld" #"\d+" "")
;; => "He Wrd"

;; Removing specific special characters
(str/replace "Hello, World! #Clojure" #"[,!#]" "")
;; => "Hello World Clojure"

;; Only keep word characters and spaces
(str/replace "Email@Example.com" #"[^\w\s]+" "")
;; => "EmailExamplecom"
```

## Deep Dive
Clojure, mirroring its Lisp heritage, excels in symbolic processing, making pattern-matching a cinch. Introduced in 2007, it builds on the Java Virtual Machine's (JVM) capabilities, utilizing Java's powerful `Pattern` class for regular expressions.

Alternatives to regex include manual string iteration and manipulation, but these are often more verbose and error-prone. Libraries like `clojure.spec` can help validate and conform data against patterns without direct deletion.

Delete operations are usually highly efficient, but be mindful of regex complexity, which can turn an O(n) task into much worse. Clojure's immutable strings mean each `replace` creates a new string, which is worth considering for memory-sensitive applications.

## See Also
- [Clojure's string API](https://clojure.github.io/clojure/clojure.string-api.html)
- [Java Pattern class](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [clojure.spec](https://clojure.org/guides/spec)
