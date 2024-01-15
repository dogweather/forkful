---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting strings to lower case is a common task in programming. Lowercasing strings can be useful for comparing inputs or creating more consistent data for processing. In Clojure, there are several ways to convert strings to lower case, making it a handy skill to have in your toolbox.

## How To

To convert a string to lower case in Clojure, you can use the `lower-case` function from the `clojure.string` library. Here's an example:

```Clojure
(require '[clojure.string :as str])

(str/lower-case "HELLO WORLD")
```

This code will output `"hello world"`. The `lower-case` function takes in a string as an argument and returns a new string with all characters converted to lower case. 

Another way to convert strings to lower case in Clojure is by using the `#(.toLowerCase %)` function. This is a Java interop function that calls the `toLowerCase()` method on a string. 

```Clojure
(#(.toLowerCase %) "HELLO WORLD")
```

This code will also output `"hello world"`. 

## Deep Dive

While the `lower-case` and `#(.toLowerCase %)` functions are the most straightforward ways to convert strings to lower case in Clojure, there are a few things to keep in mind. 

First, the `lower-case` function is case-insensitive by default. This means that it will work for all types of strings, including ones that are already in lower case. 

On the other hand, the `#(.toLowerCase %)` function is case-sensitive. This means that it will only convert uppercase characters to lowercase. If you pass in a string that is already in lowercase, it will remain unchanged.

You can also use the `repeatedly` function with the `rand-nth` function if you want to convert a string to a random combination of lowercase and uppercase characters. Here's an example:

```Clojure
(require '[clojure.string :as str])

(def s "Hello World")

(str/join "" (repeatedly #(rand-nth [(str/lower-case %) (str/upper-case %)]) s))
```

This code will output a string with random lowercase and uppercase characters, such as `"HElLo WoRLd"`.
 
## See Also

- [The clojure.string library documentation](https://clojuredocs.org/clojure.string)
- [The Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)