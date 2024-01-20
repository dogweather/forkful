---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is about merging different pieces of text into one. It is crucial to customize messages, form paths, generate outputs, and more.

## How to:

```Clojure
(let [str1 "Hello"
      str2 ", world!"]
  (str str1 str2))
```
This script outputs: `"Hello, world!"`

String concatenation is a piece of cake in Clojure, thanks to the `str` function. It accepts any number of string arguments, merges them, and gives you a brand new string. You can even throw numbers into the mix as `str` automatically converts them.

```Clojure
(let [str1 "2 + 2 = "
      num 4]
  (str str1 num))
```
The result: `"2 + 2 = 4"`

## Deep Dive

String concatenation is as old as programming itself, with many languages implementing it differently. Clojure simplifies it by giving us a versatile `str` function.

Other ways to concatenate strings in Clojure include using `clojure.string/join` function which allows you to join a collection of strings:

```Clojure
(require '[clojure.string :as str])
(str/join "-" ["Welcome" "to" "Clojure"])
```

Output: `"Welcome-to-Clojure"`

Under the hood, `str` uses StringBuilder for concatenation, which is more efficient than the naive method of repeatedly creating new strings.

## See Also

1. [Clojure - Strings](https://www.tutorialspoint.com/clojure/clojure_strings.htm)
2. [Concatenate Strings in Clojure](http://benhowell.github.io/coding/2015/05/16/concatenate-strings-in-clojure.html)
3. [Clojure Docs - str](https://clojuredocs.org/clojure.core/str)