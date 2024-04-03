---
date: 2024-01-20 17:50:29.119372-07:00
description: "String interpolation lets us toss variables into strings without a fuss.\
  \ Why? To dynamically construct text\u2014way handier than old-school string\u2026"
lastmod: '2024-03-13T22:44:59.732940-06:00'
model: gpt-4-1106-preview
summary: String interpolation lets us toss variables into strings without a fuss.
title: Interpolating a string
weight: 8
---

## What & Why?
String interpolation lets us toss variables into strings without a fuss. Why? To dynamically construct text—way handier than old-school string concatenation.

## How to:
```Clojure
;; Basics with `str` and `format`
(def name "World")
(str "Hello, " name "!")  ; => "Hello, World!"

;; Using `format`, akin to printf-style formatting
(format "Goodbye, %s!" name)  ; => "Goodbye, World!"

;; Clojure doesn't have built-in string interpolation like other languages,
;; but we can get creative with `str` and `format`.
```

## Deep Dive:
Clojure's a bit of an ascetic: no built-in string interpolation. However, `str` and `format` are the go-to for dynamic strings. Origin story? Clojure’s simplicity ethos. It trusts we can handle string construction ourselves. 

For alternatives, enter the templating world: `clostache` (a Clojure implementation of Mustache) or `hiccup` for HTML contexts. They come handy when `str` and `format` feel too primitive.

Under the hood, `format` delegates to Java’s `String.format`, a fact that illustrates Clojure’s Java interoperability superpower. So, while you don't get the sugar, you’ve got the muscle of Java when you need it.

## See Also:
- Clojure Docs on `str`: https://clojuredocs.org/clojure.core/str
- Clojure Docs on `format`: https://clojuredocs.org/clojure.core/format
- clostache GitHub repo: https://github.com/fhd/clostache
- hiccup GitHub repo: https://github.com/weavejester/hiccup
