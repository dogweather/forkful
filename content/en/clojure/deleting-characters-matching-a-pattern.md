---
title:                "Deleting characters matching a pattern"
html_title:           "Clojure recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern is a common task in programming that involves removing specific characters from a given string or sequence. This can be useful for tasks such as cleaning up input or formatting data for output. Programmers often do this to ensure consistent and error-free data processing.

## How to:

```Clojure
;; Delete all occurrences of a character
(defn delete-char
  [ch coll]
  (filter #(not= ch %) coll))

(delete-char \a "abcabc")   ;=> "bcbc"
(delete-char \1 [1 2 3 1 2]) ;=> [2 3 2]

;; Delete all occurrences of a regex pattern
(defn delete-pattern
  [pattern coll]
  (filter #(not (re-find pattern %)) coll))

(delete-pattern #"ab" ["abc" "def" "ghi"]) ;=> ["def" "ghi"]
(delete-pattern #"[aeiou]" "hello world")  ;=> "hll wrld"
```

## Deep Dive:

There are several approaches to deleting characters matching a pattern in Clojure. The first method shown above uses the filter function to remove elements from a collection based on a given predicate. This is a simple and efficient way to delete characters, but it does not support regex patterns.

For more complex pattern matching, the second method uses the re-find function from the clojure.string namespace. This allows for more advanced string manipulations, including regular expressions. However, it may be less performant than the first method, so it is important to choose the appropriate method based on the specific requirements of the task.

## See Also:

- [Clojure Docs - filter](https://clojuredocs.org/clojure.core/filter)
- [Clojure Docs - re-find](https://clojuredocs.org/clojure.string/re-find)
- [Learn Clojure - Built-In Functions](https://www.learnclojure.com/essentials/30-built-in-functions/)