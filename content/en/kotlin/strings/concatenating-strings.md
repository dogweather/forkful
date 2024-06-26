---
date: 2024-01-20 17:34:57.608654-07:00
description: 'How to: Here''s how to make strings stick together in Kotlin - no glue
  needed.'
lastmod: '2024-03-13T22:45:00.041245-06:00'
model: gpt-4-1106-preview
summary: Here's how to make strings stick together in Kotlin - no glue needed.
title: Concatenating strings
weight: 3
---

## How to:
Here's how to make strings stick together in Kotlin - no glue needed:

```kotlin
fun main() {
    val firstName = "Jet"
    val lastName = "Brains"
    val company = "Kotlin"

    // Using the plus operator
    val fullName = firstName + " " + lastName 
    println(fullName) // Output: Jet Brains

    // Using string templates
    val employeeIntro = "Hi, I'm $firstName and I work at $company."
    println(employeeIntro) // Output: Hi, I'm Jet and I work at Kotlin.

    // Using the concat() function
    val product = "IntelliJ IDEA"
    val description = " is awesome!"
    println(product.concat(description)) // Output: IntelliJ IDEA is awesome!
}
```

## Deep Dive
Concatenation has been around as long as we've had strings to tie together. Programming languages have constantly evolved the way they handle this task. In the early days, you'd find walls of text being added together with a simple `+` operator. Fast forward to modern Kotlin, and you've got templates with `$` symbols that pull variables right into the string, like magic.

Alternatives abound. If performance is key and you're dealing with a truckload of strings, StringBuilder can be your best friend, avoiding creation of multiple string objects. Then there's the `joinToString` function which takes a list and mushes it together separated by a delimiter of your choice.

Each method has its quirks—`plus` is easy but can be slow when overused; string templates are neat for readability; `concat()` harks back to Java’s method and feels a bit formal; `StringBuilder` and `joinToString` are more performant for lengthy operations.

## See Also
Dive deeper into the world of Kotlin strings:

- [Kotlin Documentation: Basic Types](https://kotlinlang.org/docs/basic-types.html#string-literals)
