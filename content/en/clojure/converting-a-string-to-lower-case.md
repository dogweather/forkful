---
title:                "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case may seem like a mundane task, but it can actually be quite useful in various programming scenarios. For example, it can be used to standardize user input, compare strings in a case-insensitive manner, or simply for aesthetic purposes.

## How To

In Clojure, converting a string to lower case is a simple task using the `clojure.string/lower-case` function. Let's take a look at some coding examples.

```Clojure 
(clojure.string/lower-case "HELLO") ;; output: "hello"
(clojure.string/lower-case "Hello World") ;; output: "hello world"
(clojure.string/lower-case "1234") ;; output: "1234"
```

We can see that the `clojure.string/lower-case` function works with not just letters, but with numbers as well. It will also preserve any non-letter characters, such as spaces or punctuation.

## Deep Dive

Under the hood, the `clojure.string/lower-case` function uses the `java.lang.String` method `toLowerCase()` to perform the conversion. This method follows the Unicode standard for case-insensitive comparisons. This means that accented characters and letters from different alphabets will be converted to their lowercase equivalents.

It's also worth noting that the `clojure.string/lower-case` function is not limited to just strings. It can also be applied to sequences of strings, such as lists or vectors. When applied to a sequence, the function will return a sequence with all elements converted to lowercase.

## See Also

To learn more about string manipulation in Clojure, check out these resources:

- [ClojureDocs - clojure.string/lower-case](https://clojuredocs.org/clojure.string/lower-case)
- [The Joy of Clojure - Chapter 2: Strings](https://www.manning.com/books/the-joy-of-clojure)
- [Clojure for the Brave and True - Chapter 7: Strings](https://www.braveclojure.com/strings/)