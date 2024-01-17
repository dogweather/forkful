---
title:                "Concatenating strings"
html_title:           "Clojure recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings in Clojure is the process of combining two or more strings into a single string. Programmers use this technique to create new strings from existing ones, allowing for more flexibility in data manipulation and text formatting.

## How to:

Concatenating strings in Clojure is done using the `str` function. This function takes in any number of string arguments and returns a new string with all the arguments combined.

```Clojure
(str "Hello, " "world!")
;; Output: "Hello, world!"
```

Concatenation can also be used with variables or other data types, as long as it can be converted to a string.

```Clojure
(def name "John")
(str "My name is " name ".")
;; Output: "My name is John."
```

## Deep Dive:

In historical context, string concatenation has been an important technique in programming since the early days. In Clojure specifically, the `str` function has been a core part of the language since its release in 2007.

Alternatives to `str` for concatenating strings include the `format` function, which allows for more control over formatting the resulting string, and the `join` function, which can concatenate a collection of strings with a specified delimiter.

In terms of implementation, Clojure's `str` function uses a `StringBuilder` under the hood, which efficiently handles the combination of strings. This results in better performance compared to other string concatenation methods.

## See Also:

- [Clojure docs on str](https://clojuredocs.org/clojure.core/str)
- [Format strings in Clojure](https://www.theserverside.com/blog/Coffee-Talk-Java-News-Stories-and-Opinions/Adding-Formatting-to-Clojure-Strings)
- [Joining strings in Clojure](https://www.clojure.com/blog/2016/12/27/joining-strings.html)