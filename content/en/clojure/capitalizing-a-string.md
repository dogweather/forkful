---
title:                "Capitalizing a string"
html_title:           "Clojure recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Capitalizing a string is a common task in programming and can be useful for various purposes such as formatting user input or generating proper titles. In Clojure, there are simple and efficient ways to achieve this task.

## How To
To capitalize a string in Clojure, we can use the `capitalize` function from the `clojure.string` library. Here's an example:

```
(require '[clojure.string :as str])

(str/capitalize "hello world") ; "Hello world"
```

In the above code, we first require the `clojure.string` library and use the `capitalize` function on the string "hello world" which returns the capitalized version of the string.

We can also use the `upper-case` and `lower-case` functions to capitalize specific characters in a string. Here's an example:

```
(str/upper-case "hello world") ; "HELLO WORLD"
(str/lower-case "HELLO WORLD") ; "hello world"
```

These functions are useful when we want to capitalize only the first character of a string or convert all characters to uppercase or lowercase.

Another approach to capitalizing a string is by using the `capitalize-first` function from the `clojure.string.capitalize` namespace. Here's an example:

```
(require '[clojure.string.capitalize :as c])

(c/capitalize-first "hello world") ; "Hello world"
```

This function capitalizes only the first letter of a string, leaving the rest unchanged.

## Deep Dive
Behind the scenes, the `capitalize` and `capitalize-first` functions use the `java.lang.String` class to achieve their functionality. This class has built-in methods for converting strings to uppercase or lowercase, which are utilized by the aforementioned functions.

Additionally, we can also use the `clojure.string/join` function along with the `map` function to capitalize each word in a string. Here's an example:

```
(require '[clojure.string :as str])

(str/join " " (map str/capitalize (str/split "hello world" #"\s"))) ; "Hello World"
```

In this code, the `split` function is used to split the string at spaces (denoted by the `#"\s"` regular expression) and return a sequence of words. This sequence is then mapped using the `capitalize` function and finally joined back together using the `join` function.

## See Also
- [Official Clojure Documentation](https://clojure.org/guides/string)
- [Clojure API Reference](https://clojure.github.io/clojure/clojure.string-api.html)
- [Clojure String Cheatsheet](https://clojure.org/api/cheatsheet)