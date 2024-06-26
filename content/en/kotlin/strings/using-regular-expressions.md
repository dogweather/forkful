---
date: 2024-02-03 19:03:08.406169-07:00
description: 'How to: To check if a string matches a specific pattern in Kotlin, you
  can use the `matches` method of the `Regex` class.'
lastmod: '2024-03-13T22:45:00.039553-06:00'
model: gpt-4-0125-preview
summary: To check if a string matches a specific pattern in Kotlin, you can use the
  `matches` method of the `Regex` class.
title: Using regular expressions
weight: 11
---

## How to:


### Basic Matching
To check if a string matches a specific pattern in Kotlin, you can use the `matches` method of the `Regex` class.

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // Output: true
```

### Finding and Extracting Parts of String
If you want to find parts of a string that match a pattern, Kotlin allows you to iterate over all matches:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "Today's date is 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// Output: 07/09/2023
```

### Replacing Text
Replacing parts of a string that match a pattern is straightforward with the `replace` function:

```kotlin
val input = "Username: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // Output: Username: userXXX
```

### Splitting Strings
Split a string into a list, using a regex pattern as the delimiter:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // Output: [1, 2, 3, 4, 5]
```

### Third-Party Libraries: Kotest
[Kotest](https://github.com/kotest/kotest) is a popular Kotlin testing library that extends Kotlin's built-in regex support, particularly useful for validation in test cases.

```kotlin
// Assuming Kotest is added to your project
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// This will pass the test if the input matches the email pattern.
```

By incorporating regular expressions into your Kotlin applications, you can perform sophisticated text processing efficiently. Whether you're validating user input, extracting data, or transforming strings, regex patterns offer a robust solution.
