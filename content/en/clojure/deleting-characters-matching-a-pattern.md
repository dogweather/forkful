---
title:    "Clojure recipe: Deleting characters matching a pattern"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

Are you tired of manually deleting unwanted characters in your code? Or do you need to clean up data by removing certain patterns? Then learning how to delete characters matching a pattern in Clojure might be the solution you've been looking for. 

## How To

Deleting characters matching a pattern in Clojure is a simple and efficient process. All you need is the `remove` function and a regular expression.

```
Clojure (def data "Hello World!")
     => #'user/data
(remove #"o" data)
     => "Hell Wrld!"
```

In the above example, we use the `remove` function to delete all instances of the character "o" from the string "Hello World!". Let's break down the code:

- `remove`: This function takes two arguments, a predicate function and a collection. In this case, we use a regular expression as the predicate function.
- `"#"`: This symbol indicates that we are using a regular expression.
- `"o"`: This is the pattern we want to match and remove from the string.
- `data`: This is the string we want to manipulate.

You can use any regular expression as a pattern to match and delete characters from a string. Let's see another example:

```
Clojure (def data [1 2 3 4 5])
     => #'user/data
(remove #"(2|4)" data)
     => (1 3 5)
```

In this example, we use a regular expression that matches either the number 2 or 4 and removes them from the vector.

## Deep Dive

Now, let's take a deeper look at the syntax and functionality of the `remove` function.

The syntax of the `remove` function is `(remove pred coll)`, where `pred` is the predicate function and `coll` is the collection.

The `remove` function traverses the collection from left to right, passing each element to the predicate function. If the result of the predicate function is truthy, the element is removed from the collection. The remaining elements are then returned as a new collection.

## See Also

To learn more about regular expressions and how to use them in Clojure, check out these resources:

- [Regular Expressions in Clojure - official documentation](https://clojuredocs.org/clojure.core/re-pattern)
- [Mastering Regular Expressions in Clojure - blog post](https://blog.marc-andre.ca/2018/08/22/mastering-regular-expressions-in-clojure/)
- [Clojure's Regular Expression Library - guide](https://www.braveclojure.com/regular-expressions/)