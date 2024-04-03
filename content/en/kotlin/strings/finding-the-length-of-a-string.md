---
date: 2024-01-20 17:47:44.518007-07:00
description: Finding a string's length means counting its characters. Programmers
  do this to validate input, loop through characters, or allocate storage.
lastmod: '2024-03-13T22:45:00.040435-06:00'
model: gpt-4-1106-preview
summary: Finding a string's length means counting its characters.
title: Finding the length of a string
weight: 7
---

## How to:
```kotlin
fun main() {
    val greeting = "Hello, World!"
    println(greeting.length)  // prints 13
}
```
Output:
```
13
```

## Deep Dive
In the early days of computing, strings were handled differently, often with null-terminated arrays in languages like C. Kotlin, as a modern language, provides a built-in `length` property for String objects.

Alternatives? Well, you could loop through a string and count characters—but why reinvent the wheel? Kotlin's `length` is efficient and simple.

Under the hood, `length` returns the count of UTF-16 code units in the string. This means that for most text (like English), the number of code units matches the number of characters. However, for characters outside the Basic Multilingual Plane (BMP), which are represented by two code units (a surrogate pair), the `length` property might not align with the number of Unicode code points.

## See Also
- Kotlin Standard Library reference for Strings: [Kotlin Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Understanding UTF-16 and character representation: [Unicode in Java](https://docs.oracle.com/javase/tutorial/i18n/text/unicode.html)
- A deep dive into Kotlin's handling of strings and related functions: [Kotlin for Java Developers](https://www.coursera.org/learn/kotlin-for-java-developers)
