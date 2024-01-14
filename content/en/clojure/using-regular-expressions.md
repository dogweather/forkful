---
title:                "Clojure recipe: Using regular expressions"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why
Regular expressions, also known as regex, are a powerful tool for text processing and manipulation. Whether you're trying to extract specific information from a large text file or validate user input in a web application, regular expressions can make your life easier by providing a concise and flexible way to search within strings.

## How To
Using regex in Clojure is simple and efficient. Let's take a look at some examples to see how it works.

```
Clojure
;; Define a string
(def input "This is a sample string 123")

;; Search for a specific pattern and return a boolean value
(re-find #"string" input)
;; Output: true

;; Search for a specific pattern and return the first occurrence
(re-find #"\d+" input)
;; Output: "123"

;; Search for a specific pattern and return all occurrences
(re-seq #"\w+" input)
;; Output: ("This" "is" "a" "sample" "string" "123")
```

As you can see, regex is used within the `re-find` and `re-seq` functions, with the first argument being the pattern to search for, and the second argument being the string to search within. The `#` symbol before the pattern indicates that it should be treated as a regex rather than a string.

## Deep Dive
Regex patterns can be as simple or as complex as you need them to be. Here are some common symbols and rules to help you build powerful regex patterns:

- `.`: matches any single character
- `*`: matches zero or more occurrences of the preceding character
- `+`: matches one or more occurrences of the preceding character
- `?`: matches zero or one occurrences of the preceding character
- `^`: matches the beginning of a string
- `$`: matches the end of a string
- `[]`: specify a range of characters to match
- `()`: group characters or patterns together
- `|`: matches either the expression to the left or the expression to the right
- `\`: escape character to match special characters like `$` and `(`

It's also important to note that regex is case-sensitive by default, but you can use the `re-matches` function to ignore case sensitivity.

## See Also
- [Clojure Regex Library Documentation](https://clojure.github.io/at-at/regex.html)
- [Regex Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Regex Testing Tool](https://regex101.com/)