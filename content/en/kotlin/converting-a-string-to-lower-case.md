---
title:                "Converting a string to lower case"
html_title:           "Kotlin recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lower case is the process of transforming all uppercase letters in a string to their lowercase equivalents. This is often done by programmers to ensure consistency in data, as well as for easier data manipulation and comparison.

## How to:
To convert a string to lower case in Kotlin, you can use the `toLowerCase()` function. Here's an example:

```
val string = "HELLO WORLD"
val loweredString = string.toLowerCase()
println(loweredString)
```

This will output: `hello world`.

You can also use the `toLowerCase()` function directly on the string itself:

```
val string = "HELLO WORLD"
println(string.toLowerCase())
```

This will produce the same output as the first example.

## Deep Dive:
Converting strings to lower case is a common programming practice that has been around for a long time. In the early days of computing, keyboards did not have both uppercase and lowercase letters, so developers had to manually convert strings to lowercase for consistent data.

There are alternative ways to convert strings to lower case, such as using regular expressions or ASCII codes. However, the `toLowerCase()` function in Kotlin is the most straightforward and efficient method.

Under the hood, the `toLowerCase()` function uses the Unicode Character Database to map all uppercase letters to their lowercase equivalents. This ensures that the conversion is accurate and supports all languages and characters.

## See Also:
- [Kotlin documentation on String Manipulation](https://kotlinlang.org/docs/strings.html)
- [Unicode Character Database](https://unicode.org/)
- [Regular Expressions in Kotlin](https://kotlinlang.org/docs/regular-expressions.html)