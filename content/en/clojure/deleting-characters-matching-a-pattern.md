---
title:                "Clojure recipe: Deleting characters matching a pattern"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

When working with text data in Clojure, it can sometimes be necessary to delete certain characters that match a specific pattern. This could be useful for removing unnecessary punctuation or formatting in a text file, or for manipulating data in a certain way.

## How To

To delete characters matching a pattern in Clojure, we can use the `replace` function from the `clojure.string` library. This function takes three arguments: the original string, the pattern to match, and the replacement string. We can use the `str/split` function to split the original string into a vector of substrings, and then use the `replace` function to replace the desired characters. Here is an example:

```Clojure
(require '[clojure.string :as str])

(def text "Hello, world!")
(def pattern #"[!;,]")

(str/replace (str/split text #" ") pattern "")
;; output: "Hello world"
```

In this example, we are using the `replace` function to replace all instances of the characters !, ;, and , with an empty string. This effectively deletes those characters from the original string.

## Deep Dive

The `replace` function in the `clojure.string` library uses regular expressions to match patterns in a string. Regular expressions are a powerful way to search for and manipulate specific text patterns. In the code example above, we used `#"[!;,]"` as our regular expression. This expression means "match any of the characters !, ;, or ,". 

Regular expressions can also use special metacharacters to match more complex patterns. For example, the metacharacter `.` means "match any single character". So, if we wanted to delete all vowels from a string, we could use `#"[aeiou]"` as our pattern. Additionally, we can use quantifiers like `+` and `*` to match one or more or zero or more of a certain character. 

For more information on regular expressions and how to use them in Clojure, check out the links in the "See Also" section below.

## See Also

- [The joy of Clojure](https://www.joyofclojure.com/) - A comprehensive guide to Clojure programming.
- [Regular Expressions in Clojure](https://clojure.org/guides/learning/resources#dogear-topic-31) - Clojure's official guide on using regular expressions.
- [Mastering Regular Expressions](https://regex.info/wisdom/regex_for_starters) - An in-depth tutorial on regular expressions.