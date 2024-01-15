---
title:                "Extracting substrings"
html_title:           "Clojure recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substrings are a fundamental part of string manipulation and are frequently used in data processing and text parsing. Extracting substrings allows for more efficient and powerful string operations, making it a valuable skill for any Clojure programmer.

## How To

To extract substrings in Clojure, we use the `subs` function. It takes in three parameters: the string itself, the starting index of the desired substring, and the ending index (which is non-inclusive).

```Clojure
(subs "Clojure is awesome" 0 6)
```

This will return the substring "Clojur". One thing to note is that the indexes in Clojure start from 0, unlike some other programming languages.

We can also use the `subs` function with negative indexes to count from the end of the string. For example, if we want to extract the last 5 characters from "Clojure is awesome", we can do:

```Clojure
(subs "Clojure is awesome" -5)
```

This will return the substring "esome".

If we omit the third parameter, `subs` will extract the substring from the starting index to the end of the string. For instance, if we only want to extract "is awesome" from our example, we can do:

```Clojure
(subs "Clojure is awesome" 8)
```

## Deep Dive

The `subs` function is just one way of extracting substrings in Clojure. There are many other functions and libraries that offer similar functionality, such as `substring`, `take`, and `drop`.

One important thing to keep in mind when using `subs` is that it returns a string, not a sequence. This means that you cannot use it in functions that expect a sequence as an input, like `map` or `reduce`. In those cases, you can use `subseq`, which returns a sequence.

Another useful tip is that we can use `subs` in conjunction with the `cl-format` function to do some advanced string formatting. For example, if we want to capitalize the first letter of each word in a string, we can do:

```Clojure
(cl-format nil "~@(capitalize (subs \"clojure is awesome\" 0 7))~@(capitalize (subs clojure is awesome\" 8 10))~@(subs \"clojure is awesome\" 10)")
```

This will return the string "Clojure Is Awesome".

## See Also

- [Clojure docs on substrings](https://clojuredocs.org/clojure.core/subs)
- [Clojure cheatsheet for string functions](https://clojure.org/guides/learn/functions)