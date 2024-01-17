---
title:                "स्ट्रिंग हींस्यकरण"
html_title:           "Clojure: स्ट्रिंग हींस्यकरण"
simple_title:         "स्ट्रिंग हींस्यकरण"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenating Strings in Clojure

## What & Why?

String concatenation is the process of combining multiple strings into a single string. Programmers often do this to create dynamic and flexible outputs, such as when generating user-specific messages or constructing URLs.

## How to:

To concatenate strings in Clojure, we use the `str` function. Below is an example of using the `str` function to concatenate two strings and print the result:

```Clojure
(str "Hello " "world!")
```

Output: `Hello world!`

We can also concatenate more than two strings by using `str` multiple times. For example:

```Clojure
(str "This " "is " (str "a " "simple ") "example.")
```

Output: `This is a simple example.`

## Deep Dive:

### Historical Context:

String manipulation has been an integral part of programming since its early days. In Clojure, the `str` function was introduced in version 0.5.0 and has been widely used by developers ever since.

### Alternatives:

Apart from using the `str` function, there are other ways to concatenate strings in Clojure, such as using the `format` function or the `+` operator. However, the `str` function is the most commonly used and efficient method.

### Implementation Details:

The `str` function takes any number of arguments and returns them as a concatenated string. It uses `StringBuilder` internally to improve performance, making it a preferred choice for string concatenation in Clojure.

## See Also:

- [Clojure Docstrings](https://clojure.org/guides/docstrings) - Clojure documentation on `str` function.
- [Clojure Cookbook](https://clojure-cookbook.com/strings/concatenating-strings) - An online resource with more examples and explanations on string concatenation in Clojure.