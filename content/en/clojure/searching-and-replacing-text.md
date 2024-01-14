---
title:    "Clojure recipe: Searching and replacing text"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why 
Searching and replacing text is a common task in programming that can save you time and effort. By automating the process, you can easily make changes to multiple files or strings without having to manually go through each one.

## How To
To search and replace text in Clojure, you can use the `replace` function with the `re-pattern` and `replace-first` functions. Here's an example that replaces all instances of "Hello" with "Hi" in a string:
```Clojure
(replace (re-pattern "Hello") "Hi" "Hello world") ; outputs "Hi world"
```
You can also use regular expressions to search for specific patterns in your text. For example, to replace all numbers with "x", you can use the `replace` function with a regular expression made up of digits:
```Clojure
(replace #(Character/isDigit %) "x" "1 2 3 4 5") ; outputs "x x x x x"
```

## Deep Dive
The `replace` function takes in three parameters: the pattern to search for, the replacement string, and the text to search in. The `re-pattern` function converts a string into a regular expression, allowing you to search for patterns rather than exact strings. The `replace-first` function is similar to `replace`, but it only replaces the first instance of the pattern. To replace all instances, you can use the `replace` function with the regular expression `#(re-seq pattern text)`.

## See Also
- [Official Clojure Documentation for `replace` function](https://clojuredocs.org/clojure.core/replace)
- [Official Clojure Documentation for `re-pattern` function](https://clojuredocs.org/clojure.core/re-pattern)
- [Official Clojure Documentation for `replace-first` function](https://clojuredocs.org/clojure.core/replace-first)