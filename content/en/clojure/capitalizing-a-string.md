---
title:                "Capitalizing a string"
html_title:           "Clojure recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string refers to the conversion of the first character of a string to uppercase. Programmers do this quite commonly for formatting purposes, for enhanced readability, and to ensure proper display of user or system data.

## How To:

Here's a quick Clojure function to handle string capitalization:

```Clojure
(defn capitalize-string [s] 
  (if (empty? s)
    ""
    (str (clojure.string/upper-case (first s)) (subs s 1))))
```

Test run it to see the result:

```Clojure
(capitalize-string "hello world!") ; results in: "Hello world!"
```

There you go - that's string capitalizing!

## Deep Dive

In Clojure, capitalizing a string is pretty straightforward, though there exists no built-in function specifically for that task. However, the nature of the language with its fundamental operations on sequences allows us to have a way around. The standard sequence operations combined with the built-in `clojure.string/upper-case` function allows for an effective implementation.

Historically, capitalizing strings is a common operation in many systems ranging from relational databases to GUI forms. It's also useful to convert user input or filenames to a standardized format.

There are different alternatives, for instance, we can use Java Interop to capitalize a string. However, idiomatic Clojure sticks to using sequences and its inbuilt functions where possible.

Implementation-wise, we combine Clojure's `first` to get the first character, `clojure.string/upper-case` to convert it to uppercase, and `subs` to get the rest of the string, finally stringing it all up together with `str`.

## See Also:

- If you need more details about Clojure's string functions you can visit [Clojure.String API](https://clojuredocs.org/clojure.string)
- For more information regarding Clojure's sequences, check out [Clojure - Sequences](https://clojure.org/reference/sequences)