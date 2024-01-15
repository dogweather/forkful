---
title:                "Using regular expressions"
html_title:           "Clojure recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool for pattern matching and text manipulation. Whether you're working with text data or need to validate user input, regular expressions can save you time and effort by providing a concise and flexible way to search and manipulate strings.

## How To

To use regular expressions in Clojure, you'll need to use the `re-find` function from the `clojure.string` library. Here's an example of using a regular expression to extract all numbers from a string:

```Clojure
(require '[clojure.string :as str])

(def text "I have 5 apples and 2 oranges.")

(re-find #"\d+" text)
;; Output: "5"
;; Only the first match is returned.

(str/split #"\d+" text)
;; Output: ["I have " " apples and " " oranges."]
;; Using the split function allows us to split the string based on the pattern, returning all the text in between the matches.

```

You can also use regular expressions to replace parts of a string. Let's say you want to censor all curse words in a given text:

```Clojure
(def text "I don't give a f**k.")

(str/replace text #"\*+" "****")
;; Output: "I don't give a ****."

```

## Deep Dive

Regular expressions use special syntax to match patterns in strings. For example, the `+` symbol matches one or more of the preceding element, `\d` matches any digit, and `\*` is an escaped asterisk.

Some other useful symbols in regular expressions include `^` for the beginning of a string, `$` for the end of a string, and `[]` for specifying a range of characters to match. For a more comprehensive list of regular expression syntax, check out this [cheat sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/) or this [quick reference guide](https://www.regular-expressions.info/quickstart.html).

It's also important to note that regular expressions can be resource-intensive, especially for large strings. So use them wisely and consider alternative methods if performance is a concern.

## See Also

If you want to learn more about regular expressions in Clojure, here are some helpful resources for further reading:

- [ClojureDocs page on `re-find`](https://clojuredocs.org/clojure.core/re-find)
- [Clojure for the Brave and True](https://www.braveclojure.com/regular-expressions/)
- [Mastering Regular Expressions by Jeffrey Friedl](https://www.amazon.com/Mastering-Regular-Expressions-Jeffrey-Friedl/dp/0596528124)