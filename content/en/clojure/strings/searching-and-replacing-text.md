---
date: 2024-01-20 17:57:33.332118-07:00
description: 'How to: In Clojure, we wield the `clojure.string/replace` function to
  search and replace text. Let''s cut to the chase with some code.'
lastmod: '2024-03-13T22:44:59.732084-06:00'
model: gpt-4-1106-preview
summary: In Clojure, we wield the `clojure.string/replace` function to search and
  replace text.
title: Searching and replacing text
weight: 10
---

## How to:
In Clojure, we wield the `clojure.string/replace` function to search and replace text. Let's cut to the chase with some code:

```clojure
(require '[clojure.string :as str])

;; Basic replacement
(str/replace "I like apples" "apples" "oranges")
;; => "I like oranges"

;; Using a regular expression to replace all vowels
(str/replace "Hello, World!" "[AEIOUaeiou]" "*")
;; => "H*ll*, W*rld!"

;; Replacement with a function for dynamic changes
(str/replace "I have 2 apples and 5 bananas"
             #"\d+"
             (fn [match] (str (inc (Integer/parseInt match)))))
;; => "I have 3 apples and 6 bananas"
```

Simple as that. Run it, and you'll see the transformations right there in your REPL.

## Deep Dive
Searching and replacing in text isn't new. It's age-old in computing. We got it from early editors like `sed` in Unix. We've come a long way since then.

Clojure, being on the JVM, means you've got Java's regular expression prowess under the hood. Performance-wise, it's nifty for quick scripts but remember, overuse in large-scale text processing can hurt performance.

As for alternatives, besides `clojure.string/replace`, there's regex-based libraries or even writing your custom function if you're feeling adventurous. Think about `replace-first` if you only need a one-shot change.

Functionally, Clojure's approach to immutability means each replacement results in a new string. No mutable strings mean fewer bugs and surprises.

## See Also
To dive deeper, check out these resources:

- Clojure's `clojure.string` [API documentation](https://clojuredocs.org/clojure.string/replace)
- On regular expressions, Java's [Pattern class](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
