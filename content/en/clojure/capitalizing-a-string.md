---
title:                "Clojure recipe: Capitalizing a string"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a simple task, but it can actually be quite useful in programming. By modifying the capitalization of a string, you can improve the readability and consistency of your code. In this blog post, we will explore how to capitalize a string in Clojure and take a deeper dive into why it is important.

## How To

The first step to capitalizing a string in Clojure is to import the string library using the `(:require [clojure.string :as str])` statement. This will give us access to the `capitalize` function, which we will use to modify our strings.

### Example 1

To simply capitalize the first letter of a string, we can use the `capitalize` function as follows:

```Clojure
(str/capitalize "hello world")
```

This will give us the output `"Hello world"`, with the first letter capitalized.

### Example 2

If we want to capitalize every word in a string, we can use the `join` and `map` functions to achieve this. Let's say we have a string called "programming is fun", and we want to capitalize every word:

```Clojure
(str/join " " (map str/capitalize (str/split "programming is fun" #"\s+")))
```

In this code, we use the `split` function to break the string into individual words, then use `map` to apply the `capitalize` function to each word, and finally use `join` to merge the words back together into a string. The output will be `"Programming Is Fun"`.

## Deep Dive

While capitalizing strings may seem like a trivial task, it can actually greatly improve the readability and consistency of your code. In a large and complex codebase, using consistent capitalization can make it easier to understand and follow the logic of the code. It also adds a level of professionalism to your code, making it more presentable and maintainable.

Another important aspect to consider is how capitalization affects string comparison. In Clojure, strings are compared using the `string=` function. If the capitalization of two strings is different, the comparison will result in `false` even if the strings are identical except for their capitalization. By capitalizing your strings consistently, you can avoid potential bugs and issues with string comparison.

It is also worth noting that Clojure has different types of strings - Unicode strings and byte strings. The `capitalize` function only works on Unicode strings, so be sure to take that into consideration when using it in your code.

## See Also

- Official Clojure documentation for the `capitalize` function: https://clojuredocs.org/clojure.string/capitalize
- Detailed explanation of string manipulation in Clojure: https://www.braveclojure.com/core-functions-in-depth/#String_Manipulation