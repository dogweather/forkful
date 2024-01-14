---
title:    "Clojure recipe: Extracting substrings"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

Substrings, or smaller strings extracted from a larger string, can be useful in many programming scenarios. For example, you might need to extract a certain part of a URL or a specific section of a file name. Being able to extract substrings can help simplify and streamline your code.

## How To

Extracting substrings in Clojure is a fairly straightforward process. First, you will need to use the built-in `subs` function, which takes in a string and a starting and ending index. For example:

```Clojure
(def str "Hello World")
(subs str 0 5) ; Output: Hello
(subs str 6 11) ; Output: World
```

In the above code, we used the `subs` function to extract the substring "Hello" by providing the starting index as 0 and the ending index as 5. Similarly, we extracted "World" by providing the starting index as 6 and the ending index as 11.

You can also use negative numbers as indices to count backwards from the end of the string. For example:

```Clojure
(def str "Hello World")
(subs str -5 -1) ; Output: Worl
```

In this case, we extracted the substring "Worl" by starting at the fifth character from the end of the string and ending at the first character from the end.

Additionally, you can use the `take` and `drop` functions to extract a specific number of characters from the beginning or end of a string, respectively. For example:

```Clojure
(def str "Hello World")
(take 5 str) ; Output: Hello
(drop 6 str) ; Output: World
```

Both `take` and `drop` also work with negative numbers as indices.

## Deep Dive

Under the hood, the `subs` function in Clojure uses the Java `String.substring` method. This means that it supports all of the same functionalities, including providing a single index to extract from that point to the end of the string. For example:

```Clojure
(def str "Hello World")
(subs str 6) ; Output: World
```

It is worth noting that unlike `take` and `drop`, `subs` does not include the character at the specified index in the extracted substring.

## See Also

- Official Clojure Documentation on `subs`: https://clojuredocs.org/clojure.core/subs
- Java `String.substring` Documentation: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-