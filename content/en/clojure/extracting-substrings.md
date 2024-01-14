---
title:                "Clojure recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substrings are an essential part of string manipulation, and being able to extract them is a crucial skill for any Clojure programmer. Whether you are working with text data or user input, extracting substrings can provide valuable insights and enable efficient data processing.

## How To

To extract a substring in Clojure, we can use the `subs` function. This function takes in three parameters - the string, the starting index, and the ending index. The starting index is inclusive, while the ending index is exclusive.

Let's see this in action:

```Clojure
(def my-string "Hello World")
(subs my-string 0 5) ; Output: "Hello"
(subs my-string 6) ; Output: "World"
```

In the first example, we extracted the substring "Hello" from the beginning of the string. In the second example, we omitted the ending index, which means it will extract all the characters from the starting index to the end of the string.

We can also use negative indexes to start from the end of the string. Let's see an example:

```Clojure
(subs my-string 0 -1) ; Output: "Hello Worl"
```

This time, we omitted the ending index, and used a negative index as the starting index to exclude the last character of the string.

## Deep Dive

There are a few things worth noting when working with the `subs` function. First, the starting and ending indexes can be either integers or ratios. Ratios are useful when you need to extract substrings relative to the length of the string. For example:

```Clojure
(subs my-string 0 (/ (count my-string) 2)) ; Output: "Hello"
```

This will extract the first half of the string, regardless of its length.

Also, it's worth mentioning that the `subs` function uses zero-based indexing, meaning the first character in the string is at index 0, the second character is at index 1, and so on. Keep this in mind when choosing your starting and ending indexes.

## See Also

- Official Clojure `subs` documentation: https://clojuredocs.org/clojure.core/subs
- A guide on string manipulation in Clojure: https://purelyfunctional.tv/article/understanding-clojure-strings/
- A useful cheat sheet for Clojure string functions: https://clojure.de/cheatsheet/strings.html