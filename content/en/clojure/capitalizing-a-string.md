---
title:    "Clojure recipe: Capitalizing a string"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

In programming, it is common to have to manipulate strings in various ways. One common task is capitalizing a string, which means converting all the characters to their uppercase equivalent. This can be useful for formatting purposes or for matching strings during comparisons.

## How To

To capitalize a string in Clojure, we can use the `clojure.string/capitalize` function. This function takes in a string as its argument and returns the same string with all characters converted to uppercase.

```Clojure
(clojure.string/capitalize "hello world")
;; Output: "HELLO WORLD"
```

If we want to capitalize only the first letter of a string, we can use the `clojure.string/capitalize-first` function.

```Clojure
(clojure.string/capitalize-first "hello world")
;; Output: "Hello world"
```

We can also use these functions when working with variables or user input.

```Clojure
(def my-string "this is a sample string")

(clojure.string/capitalize my-string)
;; Output: "THIS IS A SAMPLE STRING"

(clojure.string/capitalize-first my-string)
;; Output: "This is a sample string"
```

## Deep Dive

While the `clojure.string/capitalize` and `clojure.string/capitalize-first` functions may seem straightforward, there are some important things to keep in mind when using them. 

Firstly, these functions only work with ASCII characters. This means that any non-ASCII characters will not be capitalized, and may even result in an error. To avoid this, we can use the `clojure.string/upper-case` function, which works with all Unicode characters.

Secondly, these functions will not modify the original string, but instead return a new string with the desired changes. This is important to remember, especially when manipulating multiple strings.

## See Also

- [Clojure String Functions Documentation](https://clojuredocs.org/clojure.string)
- [Clojure String Cheat Sheet](https://github.com/zackshapiro/Clojure-String-Cheat-Sheet)

By using the `clojure.string/capitalize` and `clojure.string/capitalize-first` functions, we can easily convert strings to uppercase in our Clojure programs. Remember to consider different character sets and the immutability of strings when using these functions. Happy coding!