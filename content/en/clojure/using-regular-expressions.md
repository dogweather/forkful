---
title:                "Clojure recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are powerful tools that allow developers to manipulate and extract data from text in a precise and efficient manner. They can be used in a variety of scenarios such as data cleaning, text parsing, and string validation. Adding regular expressions to your programming toolkit can greatly enhance your ability to work with textual data.

## How To

To use regular expressions in Clojure, we first need to import the `clojure.string` library which contains helpful functions for working with strings. We can do this by including the following line at the top of our Clojure file:

```Clojure
(use 'clojure.string)
```

Let's take a simple example of extracting all the numbers from a string. We can use the `re-find` function and a regular expression pattern to do this. In the code block below, we have a string containing both alphabets and numbers. The regular expression `#"\d+"` matches any sequence of one or more digits.

```Clojure
(def example-string "abc123def456ghi789")
(re-find #"\d+" example-string)
; Output: "123"
```

We can also use regular expressions to replace parts of a string with a different value. The `replace` function takes a regular expression pattern and a replacement string as arguments. In the code block below, we replace all occurrences of the word "world" with "universe" in our example string.

```Clojure
(replace #"\bworld\b" "universe" "Hello world, hello world")
; Output: "Hello universe, hello universe"
```

## Deep Dive

Regular expressions in Clojure are written using the `#""` syntax, where the regular expression pattern is enclosed in quotes. Certain characters have special meanings in regular expressions, such as `+`, `*`, and `?`, which indicate repetition. To match these characters literally, we need to escape them using a backslash (`\`).

One of the most commonly used functions for working with regular expressions is `re-find`. This function takes a regular expression pattern and a string, and returns the first match. It also has the `re-seq` function which returns all matches in a sequence.

For a more comprehensive understanding of regular expressions in Clojure and their syntax, check out the official documentation [here](https://clojure.org/guides/learn/regular_expressions). Additionally, there are many online resources and tutorials available for learning regular expressions in general.

## See Also

- [Official documentation for regular expressions in Clojure](https://clojure.org/guides/learn/regular_expressions)
- [Regular Expressions 101](https://regex101.com/) - A useful online tool for testing and building regular expressions.
- [Mastering Regular Expressions](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/) - A comprehensive guide book to mastering regular expressions.