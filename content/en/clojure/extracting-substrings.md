---
title:                "Clojure recipe: Extracting substrings"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a common task in programming, regardless of the language being used. In Clojure, extracting substrings can be particularly useful for manipulating strings and parsing data. By understanding how to extract substrings, you can greatly enhance your ability to work with text data in Clojure.

## How To

To extract a substring in Clojure, the `subs` function is used. It takes in a string and two parameters specifying the starting and ending index of the substring. Here's an example:

```Clojure
(def str "Hello, world!")
(print (subs str 0 5))
```

In this example, we have a string "Hello, world!" and we use `subs` to extract the first 5 characters, resulting in "Hello" being printed to the console.

We can also use negative indices to specify the index from the end of the string. For example:

```Clojure
(print (subs str 7 -1))
```

This will extract the characters from index 7 (including) until the end of the string, resulting in "world!" being printed.

## Deep Dive

Clojure also provides the `subseq` function for substring extraction. Unlike `subs`, `subseq` returns a sequence rather than a string. Here's an example:

```Clojure
(def str "Hello, world!")
(print (subseq str 0 5))
```

This will return a sequence containing "H", "e", "l", "l", and "o". We can use the `join` function to convert this sequence back into a string if needed.

It's important to note that the indices used in `subs` and `subseq` are zero-based, meaning the first character is at index 0 and the last character at index -1.

## See Also

- Official Clojure Documentation for the `subs` function: https://clojuredocs.org/clojure.core/subs
- Official Clojure Documentation for the `subseq` function: https://clojuredocs.org/clojure.core/subseq