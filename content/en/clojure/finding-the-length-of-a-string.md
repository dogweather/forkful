---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string means getting the number of characters it contains. Programmers do it for validating input, allocating memory, and working with substrings.

## How to:
With Clojure, you can use the `count` function to get the length of a string. Here's an example:

```Clojure
(defn string-length [string]
  (count string))

(println (string-length "Hello, World!"))
```

When you run this code, it outputs:
```Clojure
13
```

## Deep Dive
Clojure's `count` function stems from its lineage in the Lisp family of programming languages. Historically, Lisp has no built-in string length function. But, in modern Lisps, like Clojure, you can use `count` to determine the length of a string.

You could also convert the string to a sequence using `seq`, and then use `count`. But, it's not recommended since `count` directly on a string is more efficient as `seq` results in extra overhead to create a sequence object.

Clojure implements `count` in a such way that it runs efficiently (in constant time, O(1)) for strings and other collections.

## See Also
Check out these great resources for more on Clojure strings:

1. [Clojure Strings](https://clojuredocs.org/clojure.core/string): Official Clojure docs on strings.
2. [String Functions in Clojure](https://www.learn-clojurescript.com/clojurescript/strings): Learn ClojureScript course's page dedicated to strings.
3. [Clojure by Example](https://kimh.github.io/clojure-by-example/#strings): Practical examples of Clojure code, including string manipulation.