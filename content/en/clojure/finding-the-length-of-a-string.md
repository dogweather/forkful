---
title:                "Clojure recipe: Finding the length of a string"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

One of the fundamental skills in Clojure programming is being able to manipulate strings. A common task is finding the length of a string, which can be useful for purposes such as data validation or formatting.

## How To

In Clojure, there are multiple ways to find the length of a string. The first and most straightforward method is to use the `count` function, which returns the number of characters in a string:

```Clojure
(count "Hello World") ;; returns 11
```

Another option is to use the `(.length)` method on a string, which will return the same result:

```Clojure
(.length "Hello World") ;; returns 11
```

It's also possible to use the `str` function to convert a string to a sequence and then use the `count` function:

```Clojure
(count (str "Hello World")) ;; returns 11
```

All three methods above will give the same output, but the third option may be useful if you are already working with sequences.

## Deep Dive

There are a few things to keep in mind when finding the length of a string in Clojure. Firstly, the `count` function will also work on other types of collections, such as lists or maps. This can be useful for data validation but may not always give you the expected result if you are only interested in the number of characters in a string.

Another important aspect is that the `count` function is not limited to just strings. You can pass in any data type, such as integers, and it will return the appropriate length. For example:

```Clojure
(count [1 2 3]) ;; returns 3
```

Lastly, it's worth noting that while the `count` function is a convenient and easy way to get the length of a string, it does not take into account unicode characters or multi-byte characters. If your string contains these types of characters, you may need to handle them differently to accurately measure the length.

## See Also

- [Clojure Documentation on Strings] (https://clojuredocs.org/clojure.string/length)
- [Clojure Docs on Count Function] (https://clojuredocs.org/clojure.core/count)
- [Clojure Cheat Sheet on Strings] (https://clojure.org/api/cheatsheet)