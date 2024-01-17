---
title:                "Extracting substrings"
html_title:           "Clojure recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# What & Why?
Extracting substrings is the act of retrieving a specific portion of a string, rather than the entire string. Programmers often do this to manipulate, manipulate, or analyze data in a more granular way.

# How to:
```
Clojure (str "Hello World")  ;; Retrieves the full string "Hello World"
Clojure (subs "Hello World" 0 5)  ;; Retrieves the first 5 characters "Hello"
Clojure (subs "Hello World" 6)  ;; Retrieves everything after the 6th character "World"

```

#Deep Dive:

(1) Historical context:
Extracting substrings has been a common task in programming since the early days of string manipulation. In older languages such as C and BASIC, it required writing custom code using pointer arithmetic. However, with the advent of high-level languages like Clojure, this task has become much simpler and more streamlined.

(2) Alternatives:
While extracting substrings using the `subs` function is the most common approach, there are alternative methods such as using regular expressions or the `substring` function. However, these methods may require more complex syntax and are not specific to Clojure.

(3) Implementation details:
The `subs` function in Clojure takes in three arguments - the string to be extracted from, the starting index, and the optional ending index. If no ending index is provided, the function will default to the end of the string. This makes it easy to retrieve both a single character or a range of characters from a string.

# See Also:
- [ClojureDocs - subs](https://clojuredocs.org/clojure.core/subs)
- [The Joy of Clojure - chapter 5](https://www.manning.com/books/the-joy-of-clojure-second-edition)