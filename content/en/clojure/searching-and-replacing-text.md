---
title:    "Clojure recipe: Searching and replacing text"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

At some point in your programming journey, you will encounter the need to search and replace text. Whether it's fixing typos, updating variable names, or making global changes, the ability to quickly and accurately search and replace text is a valuable skill to have.

## How To

In Clojure, there are a few ways to achieve searching and replacing text. One option is to use the `clojure.string/replace` function. This function takes three arguments: the original string, the string to search for, and the string to replace it with. Let's see an example:

```Clojure
(clojure.string/replace "Hello world!" "world" "universe")
```

This will output `"Hello universe!"`, as expected. You can also use regular expressions with the `clojure.string/replace` function, providing even more flexibility in your text replacements. Here's an example:

```Clojure
(clojure.string/replace "My lucky number is 7" #"\d+" "13")
```

This will output `"My lucky number is 13"`, as the regular expression `#"\d+"` matches any sequence of digits and replaces it with "13".

## Deep Dive

Aside from the `clojure.string/replace` function, there are other useful functions for searching and replacing text in Clojure. One such function is `clojure.string/replace-first`, which takes the same arguments as `clojure.string/replace` but only replaces the first occurrence of the search string. Additionally, you can use the `clojure.string/replace-last` function to replace the last occurrence of a string, or `clojure.string/replace-all` to replace all occurrences.

Another important concept to understand when it comes to searching and replacing text is the use of regular expressions. Regular expressions, often abbreviated as "regex", are patterns used to match and manipulate strings. They can be incredibly powerful tools for searching and replacing text, but they can also be complex and require some practice. It's worth taking the time to familiarize yourself with regular expressions and their various use cases.

## See Also

- Official Clojure documentation for `clojure.string/replace`: https://clojuredocs.org/clojure.core/replace
- Regular expressions cheat sheet: https://www.rexegg.com/regex-quickstart.html
- Example use cases for regular expressions in Clojure: https://github.com/dakrone/clojure-re-for-programmers