---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case simply means converting all the letters to their lowercase counterparts. This is often done by programmers in order to standardize text for comparison or to avoid case sensitivity issues in their code.

## How to:

```Clojure
(.toLowerCase "Hello World")
;; => "hello world"

(lower-case "AbCde")
;; => "abcde"
```

## Deep Dive:

Historically, string manipulation has been a common task for programmers. In fact, the earliest programming languages did not have built-in string manipulation functions, and developers had to come up with creative ways to accomplish this task.

In Clojure, the `lower-case` function was introduced in version 1.3.0, making it easier for developers to convert strings to lower case.

While converting a string to lower case is a simple task, it is worth noting that not all characters will be converted in the same way. For example, some languages have special rules for converting certain characters to lower case, such as the German "ß" to "ss" or the Turkish "I" to "ı." In these cases, additional steps may need to be taken to accurately convert strings to lower case.

As an alternative to using the `lower-case` function, some developers may choose to use the `(.toLowerCase)` method from Java, which Clojure is built on top of. However, the `lower-case` function is generally preferred as it is more idiomatic and also handles international characters properly.

## See Also:

- [ClojureDocs: lower-case](https://clojuredocs.org/clojure.core/lower-case)
- [JavaDocs: String toLowerCase](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [How to Use String Manipulation Functions in Clojure | Thinkster.io](https://www.thinkster.io/tutorials/clojure-string-manipulation-functions)