---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# A Quick Guide to Deleting Characters Matching a Pattern in Clojure 

## What & Why?
Deleting characters matching a pattern is an operation to remove specific characters from a string based on a provided pattern. Programmers do this to clean data, extract information, or just to implement business rules.

## How To:
In Clojure, you can use the `clojure.string/replace` function with a regular expression to delete characters. Here's an example:

```Clojure
(require '[clojure.string :as str])

(defn delete-chars [s pattern]
  (str/replace s pattern ""))

(delete-chars "Hello, world!" #"[,!]") ;; => "Hello world"
```
In this example, the characters `,` and `!` are deleted from the string `"Hello, world!"`, resulting in `"Hello world"`.

## Deep Dive
Deleting characters matching a pattern has been widely used since the earlier days of data processing. It is not exclusive to Clojure but is a feature in many other languages such as Python or Java, each language providing its unique implementation.

In Clojure, `clojure.string/replace` is not the only way to delete characters. You can also use `filter` and `clojure.string/join` to achieve the same result:

```Clojure
(defn delete-chars-2 [s pattern]
  (let [not-matching-pattern? #(not (re-matches pattern (str %)))]
    (->> s
         (filter not-matching-pattern?)
         (apply str))))

(delete-chars-2 "Hello, world!" #"[,!]") ;; => "Hello world"
```
In terms of performance, `clojure.string/replace` generally runs faster, especially for longer strings and more complex patterns. But `filter` and `clojure.string/join` might be a preferred way if you aim for a more functional style of programming.

Remember to choose the right tool for the job considering factors like readability, performance, and the nature of your specific problem.

## See Also
- [clojure.string API](https://clojure.github.io/clojure/clojure.string-api.html)
- [Clojure - Regular Expressions](https://www.tutorialspoint.com/clojure/clojure_regular_expressions.htm)