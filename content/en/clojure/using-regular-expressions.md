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

# What & Why?

Regular expressions, or regex, are sequences of characters used for pattern-matching and text manipulation. Programmers use regex to efficiently search and modify text data, making tasks such as data validation, parsing, and data extraction easier. It is a powerful tool for manipulating text-based data, allowing programmers to perform complex operations without having to write a lot of code.

# How to:

```Clojure 
; To use regular expressions in Clojure, we use the re-seq function from the clojure.string library
(require '[clojure.string :as str])

; Let's say we have a string with a list of emails that we want to extract only the valid ones
(def emails "john@gmail.com, jane@yahoo.com, steve@, tom@aol.com")

; We can use regex to validate and extract only the valid emails from the string
(str/re-seq #"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}" emails)

; Output: 
("john@gmail.com" "jane@yahoo.com" "tom@aol.com")
```

# Deep Dive:

Regular expressions have been around since the 1950s, with the first version being implemented in the programming language SNOBOL. However, it wasn't until the 1980s that regex became widely popular with the development of the Unix tool "grep." Nowadays, almost all programming languages have support for regular expressions, including Clojure.

Some alternatives to using regex include string methods such as substring and indexof, but they are limited in their capabilities and can be cumbersome for complex pattern-matching operations. Another alternative is using a parsing library, but again, this can add unnecessary complexity to simple tasks.

In Clojure, regular expressions are implemented using the Java-based library, java.util.regex. However, Clojure also has its own syntax for regex, making it more concise and easier to use than the Java version.

# See Also:

For a more in-depth understanding of regular expressions, check out the following resources:

- [Mastering Regular Expressions by Jeffrey E. F. Friedl](https://www.amazon.com/Mastering-Regular-Expressions-Jeffrey-Friedl/dp/0596528124)
- [Regular Expressions: Learn by Example](https://regexone.com/) 
- [Clojure regex cheatsheet](https://gist.github.com/telent/1180619)