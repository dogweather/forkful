---
title:    "Clojure recipe: Finding the length of a string"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

In any programming language, strings are a fundamental data type that represents a sequence of characters. As a result, it is crucial to be able to determine the length of a string for various programming tasks such as data validation, string manipulation, and program efficiency. In this blog post, we will explore how to find the length of a string in Clojure and why it is an essential skill for any programmer.

## How To

To find the length of a string in Clojure, we can use the built-in function `count`. This function takes in a collection and returns the number of elements in that collection. Since a string is essentially a collection of characters, we can use `count` to get its length.

```Clojure
(count "Hello World")
; Output: 11
```

In the example above, the string "Hello World" has 11 characters, including the space. The `count` function counts each character as an element and returns the total count.

We can also use `count` on an empty string or a string with no characters, and the output will be 0. This is because an empty string still counts as a collection.

```Clojure
(count "")
; Output: 0

(count " ")
; Output: 1
```

Another approach to finding the length of a string is by using the `length` function. This function takes in a string and returns the number of characters in that string.

```Clojure
(length "Coding in Clojure")
; Output: 17
```

While `length` can also find the length of a string, it is usually not recommended over the `count` function. The reason for this is that `count` has better performance when dealing with large collections or complex data structures.

## Deep Dive

Under the hood, the `count` function uses the `clojure.core/counted` protocol to determine the length of a given collection. This protocol is implemented in various data structures, allowing efficient and accurate counting operations.

In Clojure, strings are represented as Java objects, and the `count` function leverages the `java.lang.CharSequence` interface to count the characters in a string. The `count` function uses the `length` method from the `CharSequence` interface, making it highly efficient for string length calculations.

## See Also

- [ClojureDocs - count](https://clojuredocs.org/clojure.core/count)
- [ClojureDocs - length](https://clojuredocs.org/clojure.core/length)
- [ClojureDocs - counted protocol](https://clojuredocs.org/clojure.core/counted)