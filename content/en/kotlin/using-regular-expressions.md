---
title:                "Using regular expressions"
date:                  2024-01-19
simple_title:         "Using regular expressions"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are tools for matching patterns in text. Programmers use them to search, validate, or manipulate data efficiently.

## How to:
Kotlin makes regex easy. Let’s see some practical code examples:

```Kotlin
fun regexFind() {
    val pattern = "Kotlin".toRegex()
    val text = "Learning Kotlin is fun!"
    val matchResult = pattern.find(text)
    println(matchResult?.value) // Output: Kotlin
}

fun regexReplace() {
    val regex = "\\d+".toRegex()
    val address = "123 Main Street"
    val sanitizedAddress = regex.replace(address, "###")
    println(sanitizedAddress) // Output: ### Main Street
}

fun regexValidate() {
    val passwordPattern = "^(?=.*[A-Za-z])(?=.*\\d)[A-Za-z\\d]{8,}$".toRegex()
    val password = "Password123"
    val isPasswordValid = passwordPattern.matches(password)
    println(isPasswordValid) // Output: true
}

regexFind()
regexReplace()
regexValidate()
```

## Deep Dive
Regex has been a staple in programming since the 1950s, invented by mathematician Stephen Kleene. Alternatives to regex include string methods like `contains`, `startsWith`, or `split`, but they're less powerful. Kotlin regex is built atop Java's `Pattern` and `Matcher` classes, giving it robust performance and utility.

## See Also
- Kotlin Docs on Regex: [kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Regex Testing Tool: [regex101.com](https://regex101.com/)
- Regex Tutorial: [regular-expressions.info](https://www.regular-expressions.info/tutorial.html)
