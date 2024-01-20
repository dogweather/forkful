---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lower case means transforming all the alphabetic characters in a string from uppercase to lowercase. Programmers do this to normalize data for comparison, search, sort, or to meet specific casing requirements in text processing.

## How to:
In Kotlin, you can convert a string to lower case using the `toLowerCase()` function. Here's how:

```Kotlin
fun main() {
    val str = "HeLLo WoRLD"
    val lowerCaseStr = str.toLowerCase()
    println(lowerCaseStr) 
}
```
Sample output:

```Kotlin
hello world
```

## Deep Dive
The `toLowerCase()` function in Kotlin is not a recent development; it's been part of many programming languages, tracing back to older languages like C and Java. While it's generally the go-to method for converting a string to lower case, alternatives exist. For instance, you could manually iterate over each character in the string and convert it using `Char.toLowerCase()`.

Implementation-wise, Kotlin's `toLowerCase()` function simply calls Java's `toLowerCase(Locale.ROOT)`. This means the conversion is locale-insensitive, behaving consistently across different locales. 

```Kotlin
val str = "ÇİĞ"
println(str.toLowerCase())  // çiğ
```

Instead of producing "Çiğ", a Turkish user might expect, it produces "çiğ".

## See Also
For more information:

- [Kotlin's toLowerCase documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Locale sensitivity in string conversion](https://docs.oracle.com/javase/tutorial/i18n/locale/defLocale.html)