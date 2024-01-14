---
title:                "Clojure recipe: Concatenating strings"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

As a programming language, Clojure has a strong emphasis on immutability and functional programming. This means that instead of changing existing values, new values are created based on the original values. This approach can offer benefits such as more predictable code and easier debugging. One aspect of Clojure that may seem counterintuitive to some programmers is the use of string concatenation instead of string mutation. In this blog post, we will explore why Clojure encourages this approach and how you can effectively concatenate strings in your Clojure code.

## How To

In Clojure, strings are pieces of data just like any other value. This means that they are immutable and cannot be changed. So, if we want to combine two strings, we cannot simply modify the original strings. Instead, we need to create a new string that is the combination of the two original strings. This is where the `str` function comes in. To concatenate two strings, we can use the `str` function like this:

```Clojure
(str "Hello" "world")
```

This will return a new string "Helloworld" as the result. We can also use the `str` function with variables, like this:

```Clojure
(def greeting "Hello")
(def name "world")
(str greeting name)
```

Which will also return "Helloworld".

Another way to concatenate strings in Clojure is by using the `format` function. This is useful when you want to insert a variable into a specific location within a string. For example:

```Clojure
(format "Hello %s, how are you?" "John")
```

This will result in "Hello John, how are you?".

## Deep Dive

The reason why Clojure emphasizes string concatenation instead of mutation is due to its functional nature. By avoiding string mutation, we can create more predictable code that is easier to reason about. This is particularly important in concurrent programming, where different threads may be trying to access and modify the same strings at the same time.

Additionally, string concatenation in Clojure is more efficient than string mutation. When two strings are concatenated, a new string is created without copying the original strings. This saves memory and improves performance.

It's also worth noting that Clojure has a feature called "literal string concatenation" which allows us to simply place two strings side by side without using the `str` function. For example:

```Clojure
("Hello" "world")
```

Will result in "Helloworld". However, this feature should be used with caution as it can lead to bugs when working with non-string values.

## See Also

For more information on string concatenation in Clojure, check out the following resources:

- ClojureDocs: https://clojuredocs.org/clojure.core/str
- Clojure for the Brave and True: https://www.braveclojure.com/basic-datatypes/#Strings
- The Joy of Clojure: https://www.manning.com/books/the-joy-of-clojure-second-edition

Thank you for reading! Happy coding :)