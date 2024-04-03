---
date: 2024-01-20 17:38:50.106863-07:00
description: "Converting a string to lower case means making every character in the\
  \ string a lower-case character. Programmers do this for consistency when comparing,\u2026"
lastmod: '2024-03-13T22:45:00.036993-06:00'
model: gpt-4-1106-preview
summary: Converting a string to lower case means making every character in the string
  a lower-case character.
title: Converting a string to lower case
weight: 4
---

## What & Why?
Converting a string to lower case means making every character in the string a lower-case character. Programmers do this for consistency when comparing, sorting, or storing text.

## How to:
Kotlin's `toLowerCase()` function turns all characters in a string to lower case quickly. Here's how you use it:

```kotlin
fun main() {
    val originalString = "ThiS iS A MixED cAsE String!"
    val lowerCaseString = originalString.lowercase()

    println(lowerCaseString) // Output: this is a mixed case string!
}
```
Invoke `lowercase()` and you're done. Input caps don't matter; output's all lower case.

## Deep Dive
Kotlin didn't reinvent the wheel for lower-casing strings. It's actually a common feature across programming languages. Historically, functions like C's `tolower()` have long dealt with case conversion.

Now, two twists when lowercasing: locales and performance. Kotlin's `lowercase()` can accept a `Locale` because, surprise, character casing isn't universal. For instance, the Turkish dotted and dotless 'I' behave uniquely in case conversions.

Performance? In most apps, you won't notice. But large-scale text processing hogs more memory and time because strings in Kotlin are immutable. When you lowercase a string, you get a new string. 

Old-schoolers remember `.toLowerCase()` — Kotlin now prefers `lowercase()` for clarity.

## See Also
- Kotlin String Documentation: [Kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- For text processing and advanced case manipulation, check the `java.lang.String` API: [Oracle Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- Understanding locales and language quirks: [Oracle Locale Docs](https://docs.oracle.com/javase/tutorial/i18n/locale/)
