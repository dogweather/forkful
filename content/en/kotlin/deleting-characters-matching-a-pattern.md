---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Removing Characters Matching a Pattern In Kotlin

## What & Why?

Deleting characters matching a pattern is the process of identifying specific characters in a string and removing them. This technique comes handy in text processing, data cleaning, and modifying user inputs to meet specified conditions.

## How to:

In Kotlin, we have the replace() function for this purpose. Here's how to use it.

```Kotlin
val s = "Hello, World!"
val pattern = "[,!]".toRegex()  // Specify pattern to remove.

println(s.replace(pattern, ""))  // Outputs: Hello World
```

## Deep Dive

This simple process of deleting characters has been around since the early days of programming. It is fundamental to any language dealing with strings, including our modern-day Kotlin.

Instead of using Kotlin's replace(), you could implement your own function. Here's an example:

```Kotlin
fun String.removePattern(pattern: String): String {
    return this.filterNot { it in pattern }
}

val s = "Hello, World!"
println(s.removePattern(",!")) // Outputs: Hello World
```

This gives the same result but may be more efficient because it only iterates over the string once. However, Kotlin's replace() function is usually preferable as it is well-optimized and less prone to mistakes.

## See Also:

If you're searching for more detail or use-cases on how to work with regex in Kotlin, check these out:

- Kotlin documentation on regular expressions: [KotlinDoc: Regular Expressions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- For scenarios where you need to replace a pattern with a specific character rather than deleting it: [StackOverflow: Kotlin replace character](https://stackoverflow.com/questions/56542080/kotlin-replace-character-in-string-at-given-index)
- Tutorial on Kotlin and regex: [Baeldung: Working with regular expressions in Kotlin](https://www.baeldung.com/kotlin/regex)