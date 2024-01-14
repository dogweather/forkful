---
title:                "Clojure recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters matching a pattern can be a useful technique in several scenarios. It can help with data cleaning, removing unnecessary characters from strings, and even optimizing code performance by removing certain characters from a dataset.

## How To

In Clojure, there are a few different ways to delete characters matching a pattern. One approach is to use the `clojure.string/replace` function, which takes in a string, a regular expression pattern, and a replacement string as arguments.

```
(clojure.string/replace "Hello World!" #"[a-zA-Z]" "")
```

The above code will remove all alphabetical characters from the string and return the following output: " !". Another approach is to use the `clojure.string/replace-first` function, which only replaces the first occurrence of the pattern in the string.

```
(clojure.string/replace-first "Hello World!" #".*?" "")
```

The output of the above code will be "o World!". It is important to note that in both cases, the pattern is expressed using regular expressions, which allows for more specific and flexible matching.

## Deep Dive

In Clojure, regular expressions can be as simple or complex as needed to match specific patterns. They are denoted by the `#""` syntax and can include modifiers such as `i` for case-insensitive matching. Using regular expressions allows for powerful and precise pattern matching when deleting characters.

It is also worth mentioning that Clojure has an additional, more efficient function for deleting characters, `clojure.string/replace-s`, which uses a state-based approach and can be more performant for larger datasets.

## See Also

Here are a few resources for further reading on regular expressions and string manipulation in Clojure:

- [ClojureDocs: clojure.string](https://clojuredocs.org/clojure.string)
- [Brave Clojure: Regular Expressions](https://www.braveclojure.com/regular-expressions/)
- [Clojure Cookbook: Pattern Matching and Replacement](https://clojure-cookbook.com/strings/pattern_matching_and_replacement.html)