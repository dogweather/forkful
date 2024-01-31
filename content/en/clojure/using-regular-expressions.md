---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) search, match, and manipulate strings. They're used for their flexibility and efficiency in text processing tasks.

## How to:

```clojure
(require '[clojure.string :as str])

;; 1. Matching
(re-matches #"\d+" "123")               ;; => "123"
(re-matches #"\d+" "abc")               ;; => nil

;; 2. Searching
(re-find #"\d+" "Order 100 apples")     ;; => "100"

;; 3. Replacing
(str/replace "2023-03-15" #"\d{4}" "YYYY") ;; => "YYYY-03-15"

;; 4. Splitting
(str/split "one,two,three" #",")       ;; => ["one" "two" "three"]
```

## Deep Dive
Regular expressions have a rich history, going back to the 1950s theoretical work by Stephen Cole Kleene. Alternatives to regex include string functions like `indexOf`, `substring`, and parsing libraries; however, regex often provides a more concise solution. Clojure's regex capabilities are built-in Java's `Pattern` class, providing powerful pattern matching directly in the language.

## See Also
- [ClojureDocs on Regular Expressions](https://clojuredocs.org/clojure.core/re-find)
- [Java Pattern class](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
