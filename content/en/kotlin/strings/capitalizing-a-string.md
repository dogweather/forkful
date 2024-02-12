---
title:                "Capitalizing a string"
aliases:
- /en/kotlin/capitalizing-a-string/
date:                  2024-02-03T19:02:31.026318-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizing a string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string in programming involves converting the first character of the string to uppercase if it's not already, which is useful for formatting user inputs or displaying text in a user interface in a more standardized or human-friendly manner. Programmers perform this operation to ensure data consistency or to meet specific formatting requirements within their software applications.

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
