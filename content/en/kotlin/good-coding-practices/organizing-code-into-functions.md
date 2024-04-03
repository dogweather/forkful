---
date: 2024-01-25 02:59:56.519959-07:00
description: "How to: Here\u2019s a simple example. Instead of writing a long script\
  \ to greet users, we split the task into functions."
lastmod: '2024-03-13T22:45:00.053884-06:00'
model: gpt-4-1106-preview
summary: "Here\u2019s a simple example."
title: Organizing code into functions
weight: 18
---

## How to:
Here’s a simple example. Instead of writing a long script to greet users, we split the task into functions.

```kotlin
fun main() {
    val userName = "Alex"
    greetUser(userName)
}

fun greetUser(name: String) {
    val greeting = buildGreeting(name)
    println(greeting)
}

fun buildGreeting(name: String): String {
    return "Hello, $name! Welcome to Kotlin functions."
}

// Sample output:
// Hello, Alex! Welcome to Kotlin functions.
```

In this snippet, `greetUser` handles the action of greeting, while `buildGreeting` crafts the custom message. Small, clear roles keep things tidy.

## Deep Dive
Historically, functions stem from the mathematical concept of mapping inputs to outputs. They became programming staples because they help manage complexity, reuse code, and parallel historical structured programming paradigms, like those in C.

Alternatives? Some prefer OOP (Object-Oriented Programming) where you encapsulate functions into classes. Others like FP (Functional Programming) which pushes stateless functions and immutability. Kotlin plays nice with both.

Implementation details matter. How you name your functions, how many parameters they have, and what they return can seriously affect readability and maintainability. Plus, things like scope, visibility, and higher-order functions bring extra power to your coding toolkit in Kotlin.

## See Also
Dive deeper with these resources:
- Kotlin Documentation on functions: [kotlinlang.org/docs/functions.html](https://kotlinlang.org/docs/functions.html)
- "Clean Code" by Robert C. Martin, particularly the sections on functions.
- FP concepts in Kotlin:
  [kotlinlang.org/docs/fun-interfaces.html](https://kotlinlang.org/docs/fun-interfaces.html)
- A look into OOP in Kotlin:
  [kotlinlang.org/docs/object-oriented-programming.html](https://kotlinlang.org/docs/object-oriented-programming.html)
