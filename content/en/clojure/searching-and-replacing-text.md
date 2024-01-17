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

## What & Why?
Searching and replacing text is the process of finding a specific pattern of characters in a piece of text and replacing it with another pattern of characters. Programmers often use this technique to automate the process of making changes to large amounts of code or data.

## How to:
To search and replace text in Clojure, we can use the `str/replace` function. This function takes three arguments: the string we want to search, the pattern we want to find, and the replacement pattern. For example, to replace all instances of "hello" with "hi" in the string "hello world," we can use the following code:

```Clojure
(str/replace "hello world" "hello" "hi") 
```

The output of this code would be "hi world."

We can also use regular expressions as the search pattern. This allows for more complex and dynamic replacements. For example, to replace all numbers in a string with "x," we can use the regular expression `#"\d+"` and the following code:

```Clojure
(str/replace "I have 123 apples." #"\d+" "x")
```

The output of this code would be "I have x apples."

## Deep Dive:
Searching and replacing text has been a common task in programming for many years. In the early days of text editors, programmers had to manually search and replace text in their code. This was a time-consuming and error-prone process. The development of regular expressions and the creation of search and replace functions in programming languages, such as Clojure, has greatly improved this process.

In Clojure, there are other functions and macros available for searching and replacing text, such as `str/replace-first` and `clojure.string/replace-first`, which allow for more targeted replacements. Additionally, there are also libraries, such as `clojure.string` and `str-utils`, that provide more advanced text manipulation functions.

When using regular expressions for search and replace in Clojure, it's important to keep in mind the syntax and the different "flavors" of regular expressions. Clojure uses the Java regular expression library, which has its own set of syntax rules. Other programming languages may use different rules, so it's important to be aware of this when using regular expressions in cross-language projects.

## See Also:
- [Clojure documentation for str/replace](https://clojuredocs.org/clojure.string/replace)
- [Regular expression cheat sheet](https://www.rexegg.com/regex-quickstart.html)