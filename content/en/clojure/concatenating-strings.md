---
title:                "Clojure recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

When writing code in Clojure, you may often come across the need to combine or "concatenate" strings together. This is a common task in programming and allows us to manipulate and display text in various ways. In this blog post, we will explore the why and how of concatenating strings in Clojure.

## How To

To concatenate strings in Clojure, we can use the `str` function. This function takes in one or more arguments and returns a string that contains all of the arguments concatenated together. Here is an example of how to use `str`:

```Clojure
(str "Hello" ", " "world!") ; Output: "Hello, world!"
```

We can also use string interpolation to concatenate strings. String interpolation allows us to include variables or expressions within a string. We do this by using the `format` function along with `%` symbols to represent where the variables or expressions should be placed. Here is an example:

```Clojure
(let [name "John"]
  (format "Hello, %s!" name)) ; Output: "Hello, John!"
```

Another useful function for concatenating strings is `join`. This function takes in a collection and a separator, and returns a string with all the elements of the collection joined together using the separator. Here is an example:

```Clojure
(join "-" ["apple" "orange" "banana"]) ; Output: "apple-orange-banana"
```

Lastly, we can also use the `clojure.string/join` function to concatenate strings. This function is similar to `join`, but it takes in a sequence of strings instead of a collection. Here is an example:

```Clojure
(clojure.string/join " and " ["Clojure" "Java" "Python"]) ; Output: "Clojure and Java and Python"
```

Now that we have seen some examples of how to concatenate strings in Clojure, let's take a deeper dive into this topic.

## Deep Dive

As we have seen, there are multiple ways to concatenate strings in Clojure. The `str` function is the most commonly used one, but it is important to also be familiar with string interpolation and the `join` functions. These different methods allow us to have flexibility in how we combine strings and choose the one that best suits our needs.

It is important to note that when using `str` or string interpolation, the arguments must be strings or they will be converted to strings. In the case of `join` or `clojure.string/join`, the collection or sequence can contain any type of data, and they will be converted to strings before being joined together.

Concatenating strings in Clojure is a powerful tool, but it is also important to be mindful of performance when working with large strings. Each time a string is concatenated, a new string is created in memory, which can impact performance if done repeatedly.

## See Also

- Official documentation for `str`: https://clojuredocs.org/clojure.core/str
- Official documentation for `format`: https://clojuredocs.org/clojure.core/format
- Official documentation for `join`: https://clojuredocs.org/clojure.core/join
- Official documentation for `clojure.string/join`: https://clojuredocs.org/clojure.string/join