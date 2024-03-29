---
date: 2024-01-20 17:51:05.824126-07:00
description: String interpolation lets you embed variables directly in strings. It's
  handy for creating dynamic, readable text without clunky concatenation.
lastmod: '2024-03-13T22:45:00.036167-06:00'
model: gpt-4-1106-preview
summary: String interpolation lets you embed variables directly in strings. It's handy
  for creating dynamic, readable text without clunky concatenation.
title: Interpolating a string
---

{{< edit_this_page >}}

## What & Why?
String interpolation lets you embed variables directly in strings. It's handy for creating dynamic, readable text without clunky concatenation.

## How to:
```kotlin
fun main() {
    val name = "Alex"
    val age = 29
    // Interpolate variables into the string
    val greeting = "Hello, my name is $name and I am $age years old."
    println(greeting) // Output: Hello, my name is Alex and I am 29 years old.

    // Expressions within strings
    val announcement = "Next year, I'll be ${age + 1}!"
    println(announcement) // Output: Next year, I'll be 30!
}
```

## Deep Dive
Kotlin, influenced by other modern languages, introduced string interpolation as a cleaner alternative to Java's string concatenation. It improves readability and simplifies code.

Historically, Java required verbose concatenation using `+`, which could be both hard to read and less efficient, as it created multiple string objects. Kotlin’s approach is more powerful, allowing not just variable embedding but also expression evaluation within strings.

Under the hood, Kotlin compiles this interpolation into `StringBuilder` operations or string concatenation, depending on the complexity, taking the burden off the developer. 

Alternatives to string interpolation include template engines for extensive text manipulation, but in code, interpolation is generally the quickest way to include dynamic content.

## See Also
- [Kotlin Documentation on String Templates](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [Kotlin's `String` API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Comparing Java and Kotlin String concatenation performance](https://proandroiddev.com/the-cost-of-kotlin-language-features-8f7035e9dcb9)
