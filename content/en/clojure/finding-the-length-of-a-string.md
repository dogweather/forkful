---
title:                "Clojure recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

When programming in Clojure, you may come across the need to find the length of a string. This is a common task that is necessary for various reasons such as data validation and manipulation.

## How To

Finding the length of a string in Clojure is a simple task. You can use the `count` function to return the number of characters in a string. Let's see an example:

```Clojure
(def str "Hello World")
(count str)

;; Output: 11
```

In the above code, we first create a string variable `str` with the value "Hello World". Then, we use the `count` function to get the length of the string and print it out as an output. It's that easy!

You can also use the `count` function on any data type that implements the `counted` interface, such as lists and vectors. Let's see how it works on a vector:

```Clojure
(def vec [1 2 3 4 5])
(count vec)

;; Output: 5
```

In addition, you can use the `seq` function to convert a string into a sequence of characters and then use the `count` function to get the length. Let's try this out:

```Clojure
(def str "Hello World")
(count (seq str))

;; Output: 11
```

## Deep Dive

In Clojure, strings are represented as sequences of characters, which means you can use all sequence functions on strings. This is why the `count` function works on strings and other data types that implement the `counted` interface.

However, it's important to note that the `count` function has a linear time complexity, which means the time it takes to find the length of a string increases as the string gets longer. This is something to consider when dealing with large strings in your code.

## See Also

- [Clojure string functions](https://clojuredocs.org/clojure.string)
- [Clojure sequence functions](https://clojuredocs.org/sequences) 
- [Clojure counted interface](https://clojuredocs.org/clojure.core/counted)