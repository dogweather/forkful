---
title:    "Clojure recipe: Using regular expressions"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

In today's fast-paced world of programming, efficiency is key. Regular expressions are a powerful tool that can help you quickly manipulate and search through text data, making them an essential skill for any Clojure programmer. They allow you to match specific patterns within a larger text and apply various transformations to the matched text. This not only saves you time and effort, but also allows for more precise and accurate text processing.

## How To

Using regular expressions in Clojure is relatively straightforward. Let's take a look at some examples using the `re-find` function.

To start, we need to use a regular expression literal, which is made up of a pattern enclosed in forward slashes. Let's say we have a string containing various phone numbers and we want to extract just the area code. We can use the `\d{3}` pattern to match three consecutive digits and use `re-find` to extract the first occurrence of it:

```Clojure
(def phone-numbers "Call me at 111-222-3333 or 444-555-6666")
(re-find #"\d{3}" phone-numbers)
```
Output: "111"

We can also use capturing groups to extract specific parts of a matched text. For example, let's say we have a string of email addresses and we want to extract just the domain name. We can use the `re-find` function with the `#"(?<=@)\w+"` pattern, which matches any word characters preceded by the "@" symbol:

```Clojure
(def emails "john.doe@example.com, jane.smith@example.net")
(re-find #"(?<=@)\w+" emails)
```
Output: "example"

There are many more ways to use regular expressions in Clojure, such as replacing or manipulating text, as well as more complex matching patterns. It's important to experiment and practice with regular expressions to fully harness their power.

## Deep Dive

Regular expressions can be intimidating at first, as they require you to think in terms of patterns and matching rather than just individual characters. However, once you understand the basics and gain some experience, they can become an invaluable tool in your programming arsenal.

One thing to keep in mind is that regular expressions are case sensitive by default, but you can use the `(?i)` flag to make them case insensitive. Additionally, there are many useful functions in Clojure's `clojure.string` library that work well with regular expressions, such as `clojure.string/replace` and `clojure.string/split`.

It's also important to be aware of any potential performance issues when using regular expressions, as they can be resource-intensive for certain tasks. In these cases, it may be more efficient to use a different approach or optimize your regular expressions.

## See Also

- Official ClojureDocs for `re-find`: https://clojuredocs.org/clojure.core/re-find
- Regular Expressions Cheat Sheet: https://www.rexegg.com/regex-quickstart.html
- Regular Expressions in Clojure: https://blog.cognitect.com/clojure/2017/02/13/regular-expressions-in-clojure.html