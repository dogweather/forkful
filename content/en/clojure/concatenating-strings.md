---
title:    "Clojure recipe: Concatenating strings"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a common operation in programming, especially when it comes to creating user-friendly output. In Clojure, there are different ways to concatenate strings depending on the specific task at hand. Understanding these methods is essential for anyone looking to write efficient and effective Clojure code.

## How To

There are a few ways to concatenate strings in Clojure, each with their own advantages and use cases.

One simple way is to use the `str` function, which takes in any number of arguments and returns them concatenated as a string. For example:

```Clojure
(str "Hello" " " "World")
```

Running this code would output `"Hello World"`.

Another method is to use the `clojure.string/join` function, which takes in a collection of strings and joins them together with a specified delimiter. For example:

```Clojure
(clojure.string/join ", " ["apple" "banana" "orange"])
```

This would output `"apple, banana, orange"`.

Lastly, the `StringBuilder` class from Java can be used to concatenate strings for more complex scenarios. It allows for efficient string building and manipulation, making it a useful tool in performance-critical situations.

## Deep Dive

It's important to note the subtle differences between the different methods of string concatenation in Clojure. Using the `str` function is the most efficient and recommended way for simple concatenations. However, when dealing with large collections of strings or the need for a specific delimiter, it's more efficient to use `clojure.string/join` instead.

Additionally, it's worth exploring the `StringBuilder` class for more complex string operations. Its `append` and `insert` methods provide efficient ways to manipulate strings, making it a valuable tool for certain use cases.

## See Also

- Official Clojure Documentation on Strings: https://clojure.org/guides/strings
- ClojureDocs page for `str` function: https://clojuredocs.org/clojure.core/str
- ClojureDocs page for `clojure.string/join` function: https://clojuredocs.org/clojure.string/join
- Java StringBuilder Class Documentation: https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html