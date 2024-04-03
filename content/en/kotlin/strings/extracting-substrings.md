---
date: 2024-01-20 17:45:54.335545-07:00
description: 'How to: In Kotlin, use `substring`, `take`, and `drop` functions.'
lastmod: '2024-03-13T22:45:00.038723-06:00'
model: gpt-4-1106-preview
summary: In Kotlin, use `substring`, `take`, and `drop` functions.
title: Extracting substrings
weight: 6
---

## How to:
In Kotlin, use `substring`, `take`, and `drop` functions.

```Kotlin
fun main() {
    val text = "Hello, Kotlin!"

    println(text.substring(7, 13)) // Prints "Kotlin"
    
    // From start
    println(text.take(5)) // Prints "Hello"

    // From end
    println(text.takeLast(6)) // Prints "Kotlin!"

    // Dropping chars
    println(text.drop(7)) // Prints "Kotlin!"
}
```

## Deep Dive
In the early days of programming, handling strings was manual and error-prone. In Kotlin, it's easier, safer, and less resource-intensive, thanks to built-in functions and String class features.

Alternatives to `substring` include using regular expressions with `Regex` or `split` to dice up strings—but these methods can be overkill for simple tasks.

Implementation-wise, remember that strings are immutable in Kotlin. So, when you extract a substring, you're actually creating a new String object, not changing the original.

## See Also
- Kotlin String documentation: [Kotlin Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Regex in Kotlin for advanced string manipulation: [Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
