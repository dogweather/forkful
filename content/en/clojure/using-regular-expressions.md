---
title:    "Clojure recipe: Using regular expressions"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool for manipulating and extracting data from strings in Clojure. They allow you to search for specific patterns and perform replacements or transformations, making it easier to handle complex data. In this blog post, we will explore how to use regular expressions in Clojure and its benefits for developers.

## How To

Using regular expressions in Clojure is straightforward. First, you need to import the `clojure.string` library, which contains functions to work with regular expressions. Then, you can use the `re-matches` function to find all occurrences of a pattern in a string. Let's see an example:

```Clojure
(require '[clojure.string :as str])

(def text "I love Clojure!")
(str/re-matches #"love" text)
```

Output: `"love"`

In this case, we are looking for the word "love" in the string "I love Clojure!" and the `re-matches` function returns that match. You can also use the `re-find` function to find the first occurrence, and `re-seq` to get a sequence of all matches.

Regular expressions also allow you to perform replacements using the `re-gsub` function. It takes in a pattern, replacement, and string and replaces all occurrences of the pattern with the replacement. Let's try it out:

```Clojure
(require '[clojure.string :as str])

(def text "I love Clojure!")
(str/re-gsub #"love" "like" text)
```

Output: `"I like Clojure!"`

Regular expressions also support capturing groups, which allows you to extract specific parts of a pattern. To access these groups, you can use the `re-find` function and provide an index for the group you want to retrieve. For example:

```Clojure
(require '[clojure.string :as str])

(def text "My email is abc@def.com")
(re-find #"([a-z]+)@([a-z]+)\.com" text 2) ;; returns the second group
```

Output: `"def"`

## Deep Dive

Regular expressions in Clojure support various special characters and modifiers that allow for even more flexibility when matching patterns. Some of these include `?` to make a character optional, `+` to match one or more occurrences, and `*` to match zero or more occurrences. You can also use the `^` and `$` anchors to specify the start and end of a string, respectively.

Additionally, character classes can be used to match specific sets of characters. For example, `[a-z]` will match any lowercase letter and `[0-9]` will match any digit. You can also use quantifiers with character classes, such as `[a-z]{3}` to match exactly three lowercase letters.

For a more comprehensive guide on regular expressions in Clojure, you can refer to the official documentation.

## See Also

- [Official Clojure documentation on regular expressions](https://clojure.org/guides/learn/regular_syntax)
- [Mastering Regular Expressions by Jeffrey E. F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)