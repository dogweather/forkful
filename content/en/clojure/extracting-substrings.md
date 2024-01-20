---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is the task of getting a smaller string (substring) from a larger one in programming. We do this because it's a handy way to drill down into text data, whether for extracting unique ID's, parsing useful parts, or simply performing text manipulations.

## How to:

Let's see how to extract a substring in Clojure:

```clojure
(subs "Clojure is Awesome!" 0 7)
```

This will return "Clojure" because indices in Clojure start from 0, and the `subs` function extracts the substring from the start index to the index before the end.

If you want to extract from a certain point to the end, just give the start index.

```clojure
(subs "Clojure is Awesome!" 9)
```

This will output "is Awesome!" because we're starting from index 9 till the end of the string.

## Deep Dive:

Originally arising from the Lisp tradition, Clojure represents strings as Java String objects but intermingles the idea of substrings from the general sphere of programming. The `subs` function lets you take chunks out of strings, conceptually akin to slices in other languages like Python.

Alternatives for substring extraction in Clojure exist, though. You might convert a string into a sequence of characters and play with Clojure's rich sequence functions. Remember, though, that `subs` is direct and usually more efficient.

On the implementation side, Clojure's `subs` function uses Java's `substring` method, which creates a new string object that references the same char array as the original string but with different offset and count values.

## See Also:

1. Clojure Docs: [subs](https://clojuredocs.org/clojure.core/subs)
2. Stack Overflow: [Clojure String to Character Array](https://stackoverflow.com/questions/5427764/clojure-string-to-character-array) for alternatives.
3. Java Docs: [substring](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int)) for the implementation journey of `subs`.