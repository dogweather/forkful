---
title:    "Clojure recipe: Extracting substrings"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself needing to extract certain parts of a string in your Clojure program? Maybe you want to manipulate a specific word or set of characters within a larger string. If so, then this blog post is for you! In this article, we will explore how to extract substrings in Clojure and why it can be a useful tool in your programming toolkit.

## How To

First, let's start by defining a string that we want to extract a substring from:

```Clojure
(def my-string "Hello, Clojure!")
```

To get a specific substring from this string, we can use the `subs` function and specify the starting and ending indexes of the substring we want:

```Clojure
(subs my-string 7 14)
```

This will return the substring "Clojure" from our original string. 

Another way to extract substrings is by using regular expressions. We can use the `re-find` function to search for a specific pattern within a string and extract the substrings that match that pattern. For example, if we want to extract all the vowels from our original string, we can do the following:

```Clojure
(re-find #"[aeiou]" my-string)
```

This will return a list of all the vowels in our string.

## Deep Dive

Under the hood, the `subs` function uses the `subvec` function to extract the substring. The `subvec` function is similar to the `subarray` function in other programming languages, where it takes a vector as input and returns a portion of that vector based on the specified indexes. This explains why we can also use `subs` on vectors and get the same results.

It is also worth noting that the `subs` function is inclusive of the starting index but exclusive of the ending index. This means that the character at the ending index will not be included in the returned substring.

## See Also

For more information on substrings and other string manipulation functions in Clojure, check out the official documentation here: https://clojuredocs.org/clojure.core/subs

You can also explore regular expressions in more detail and see how they can be used for extracting substrings in Clojure here: https://clojuredocs.org/clojure.core/re-find