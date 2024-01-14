---
title:                "Clojure recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is an essential task in any programming language, including Clojure. It allows you to quickly modify large amounts of text, saving you both time and effort. With the help of Clojure's powerful string functions, this task becomes even more efficient and seamless. In this blog post, we will explore the various techniques and functions for searching and replacing text in Clojure.

## How To

To search and replace text in Clojure, we will be using the `replace` function from the `clojure.string` namespace. The basic syntax for using this function is:

```
(replace text search & args)
```

Let's now look at some examples to understand how this function works.

### Simple Search and Replace

Suppose we have a string containing the phrase "Hello World" and we want to replace "World" with "Universe". We can use the `replace` function as follows:

```Clojure
(clojure.string/replace "Hello World" "World" "Universe")
```

The output of this code will be:

```
"Hello Universe"
```

### Using Regex

Clojure's `replace` function supports regex (regular expression) patterns, allowing us to perform more complex searches and replacements. For example, we can use the regex pattern `#"[aeiou]"` to replace all vowels in a string with asterisks.

```Clojure
(clojure.string/replace "Hello World" #"[aeiou]" "*")
```

The output of this code will be:

```
"H*ll* W*rld"
```

### Multi-line Search and Replace

The `replace` function also supports multi-line search and replace. To do so, we can use the `clojure.string/replace-lines` function. Let's say we have the following string:

```
"Programming
is
fun"
```

And we want to replace all instances of "is" with "can be". We can use `replace-lines` as follows:

```Clojure
(clojure.string/replace-lines "Programming
is
fun"
"is" "can be")
```

The output of this code will be:

```
"Programming
can be
fun"
```

## Deep Dive

Clojure's `replace` function uses a highly optimized hybrid implementation of Boyer-Moore and Knuth-Morris-Pratt algorithms to efficiently search and replace text. This makes it a performant and reliable choice for manipulating large amounts of text.

There are also other string functions in Clojure, such as `replace-first` and `replace-regexp`, that offer additional features for more specific use cases. These functions can be explored in more depth in the official Clojure documentation.

## See Also

- [Official Clojure Documentation for replace](https://clojuredocs.org/clojure.string/replace)
- [Clojure string functions cheatsheet](https://clojure.org/api/cheatsheet#Strings)
- [Practical guide to Clojure string manipulation](https://practicalclj.github.io/string-manipulation/)