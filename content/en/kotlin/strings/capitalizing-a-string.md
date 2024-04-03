---
date: 2024-02-03 19:02:31.026318-07:00
description: "How to: In Kotlin, strings can be capitalized using the standard library\
  \ functions without the need for third-party libraries. Kotlin's approach to\u2026"
lastmod: '2024-03-13T22:45:00.033521-06:00'
model: gpt-4-0125-preview
summary: In Kotlin, strings can be capitalized using the standard library functions
  without the need for third-party libraries.
title: Capitalizing a string
weight: 2
---

## How to:
In Kotlin, strings can be capitalized using the standard library functions without the need for third-party libraries. Kotlin's approach to handling strings makes these operations straightforward and concise.

### Capitalizing the entire string:
```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // Output: HELLO, WORLD!
```

### Capitalizing only the first character:
As of Kotlin 1.5, the `capitalize()` function is deprecated and replaced with a combination of `replaceFirstChar` and a lambda that checks if it is a lower case letter to transform it to uppercase.

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // Output: Hello, world!
```

This approach maintains the rest of the sentence in its original form while only changing the first letter to uppercase.
