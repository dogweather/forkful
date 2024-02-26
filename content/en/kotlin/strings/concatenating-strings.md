---
date: 2024-01-20 17:34:57.608654-07:00
description: "String concatenation is like making a sandwich, but instead of bread\
  \ and fillings, you're stacking words together to form a sentence or a phrase.\u2026"
lastmod: '2024-02-25T18:49:56.486938-07:00'
model: gpt-4-1106-preview
summary: "String concatenation is like making a sandwich, but instead of bread and\
  \ fillings, you're stacking words together to form a sentence or a phrase.\u2026"
title: Concatenating strings
---

{{< edit_this_page >}}

## What & Why?

String concatenation is like making a sandwich, but instead of bread and fillings, you're stacking words together to form a sentence or a phrase. Programmers concatenate to create dynamic texts, like showing a user's name with a greeting, or crafting file paths on-the-fly.

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
