---
date: 2024-01-20 17:56:19.601839-07:00
description: 'How to: In the code above, `args` is an array holding the command line
  arguments. The `main` function checks if we got any, and greets accordingly.'
lastmod: '2024-04-05T21:53:35.760659-06:00'
model: gpt-4-1106-preview
summary: In the code above, `args` is an array holding the command line arguments.
title: Reading command line arguments
weight: 23
---

## How to:
```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Hello, ${args[0]}!")
    } else {
        println("Hello, unknown person!")
    }
}

// Sample Output if passed 'Kotlinista' as an argument:
// Hello, Kotlinista!
```

In the code above, `args` is an array holding the command line arguments. The `main` function checks if we got any, and greets accordingly.

## Deep Dive
The concept of command line arguments is old as hills; it's been a part of programming since the dawn of time—or at least since the creation of interactive terminals. In the context of Kotlin, which runs on the JVM, command line arguments work similarly to Java. 

Other languages offer similar means, like `argv` in Python or `$argc` and `$argv` in PHP. Kotlin's approach keeps it simple—the `main` function just takes an `Array<String>`.

As for implementation details, remember that array indices start at zero. `args[0]` is the first argument, `args[1]` is the second, and so on. Also, bear in mind that if you're building a complex app that needs to parse commands more flexibly, you might want to look into a dedicated library like kotlinx-cli.

## See Also
- [Kotlin's Official Documentation on Command-Line Applications](https://kotlinlang.org/docs/command-line.html)
- [kotlinx-cli on GitHub](https://github.com/Kotlin/kotlinx-cli)
