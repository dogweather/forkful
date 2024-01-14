---
title:    "Clojure recipe: Concatenating strings"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

Concatenating strings is a fundamental operation in many programming languages, including Clojure. It allows you to combine multiple strings into a single string, making it easier to read and manipulate data in your program. This can be particularly useful when working with text-heavy applications, such as data processing or web development.

## How To

To concatenate strings in Clojure, we can use the `str` function, which takes in one or more strings as arguments and returns a new concatenated string. We can also use the `clojure.string/join` function, which takes in a separator as the first argument and a collection of strings as the second argument. Let's see some examples below:

```
Clojure

(def str1 "Hello")
(def str2 "World")

(str str1 str2)
;; Output: "HelloWorld"

(clojure.string/join " " [str1 str2])
;; Output: "Hello World"
```

We can also concatenate strings using the `+` operator, but it is generally considered a good practice to use the `str` or `clojure.string/join` functions instead.

## Deep Dive

When using the `str` function, it is important to note that the arguments are concatenated in the order they are passed in. This means that the type of the first argument determines the return type. For example, if the first argument is a string, then the returned value will also be a string. However, if the first argument is a number, then the returned value will be a string representation of that number followed by the remaining arguments.

Additionally, when using the `clojure.string/join` function, the separator can be any string, not just a space. This makes it useful for more complex concatenations, such as CSV files or HTML tables.

## See Also

- Official Clojure documentation on `str`: https://clojure.org/reference/data_structures#_string_functions
- Official Clojure documentation on `clojure.string/join`: https://clojuredocs.org/clojure.string/join
- Clojure Tutorial on string manipulation: https://www.braveclojure.com/working-with-strings/