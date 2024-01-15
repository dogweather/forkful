---
title:                "Searching and replacing text"
html_title:           "Clojure recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself manually replacing text in a document or code, only to realize you missed a few instances and have to go back and fix them? That's where searching and replacing text comes in handy. It allows you to quickly and efficiently replace text in a document or code with just a few lines of code.

## How To

To search and replace text in Clojure, you can use the `replace` function from the `clojure.string` library. This function takes in three parameters: the string to search in, the pattern to search for, and the replacement text. Let's take a look at an example:

```
```Clojure
(require '[clojure.string :as str])

(def str-to-replace "Hello World!")

(str/replace str-to-replace "World" "Universe")
```

In this example, we first require the `clojure.string` library and bind it to the alias `str`, just for convenience. Then we define a string variable called `str-to-replace` with the value "Hello World!". Finally, we use the `replace` function to replace the word "World" with "Universe" in our string variable.

The output of this function call would be the string "Hello Universe!", since it replaces all instances of "World" with "Universe" in the original string.

## Deep Dive

The `replace` function supports regular expressions as the pattern to search for, giving you even more flexibility in your search and replace operations. It also has optional parameters for specifying the maximum number of replacements and the starting index to search from.

Keep in mind that the `replace` function is not limited to strings, but can also be used on other data types such as vectors and maps. This makes it a versatile tool for all your searching and replacing needs.

## See Also

- [Clojure Docs on replace function](https://clojuredocs.org/clojure.string/replace)
- [ClojureRegex - a helpful library for working with regular expressions in Clojure](https://github.com/luxbock/clojure-regex)