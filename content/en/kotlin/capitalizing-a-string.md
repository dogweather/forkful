---
title:                "Kotlin recipe: Capitalizing a string"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why Capitalize a String in Kotlin

Capitalizing a string is a common task in programming, especially when dealing with user input or manipulating data. By capitalizing a string, you can ensure consistency and improve readability in your code. In Kotlin, there are a few different ways to capitalize a string, and in this blog post, we will explore them.

## How To Capitalize a String in Kotlin

There are a few different ways to capitalize a string in Kotlin. Let's take a look at each one and see some examples of their output.

First, we can use the `toUpperCase()` method on a string. This will convert all characters in the string to uppercase letters.

```Kotlin
val name = "jane doe"
println(name.toUpperCase()) // Output: JANE DOE
```

Next, we can use the `capitalize()` method on a string. This will capitalize only the first letter of the string, leaving the rest of the characters as is.

```Kotlin
val city = "new york"
println(city.capitalize()) // Output: New york
```

We can also use the `replaceFirstChar()` method to capitalize the first letter of a string.

```Kotlin
val country = "britain"
println(country.replaceFirstChar { it.uppercase() }) // Output: Britain
```

## Deep Dive into Capitalizing a String

In Kotlin, strings are immutable, meaning they cannot be changed. This means that when you capitalize a string, a new string is created with the capitalized letters. It is important to keep this in mind when working with strings in Kotlin.

Additionally, depending on the language and locale settings, the output of the `capitalize()` and `toUpperCase()` methods may vary. For example, in German, the letter "ß" may be converted to "SS" when using `toUpperCase()`, but remain as "ß" when using `capitalize()`.

## See Also

- [Kotlin Strings](https://kotlinlang.org/docs/strings.html)
- [Kotlin Standard Functions](https://kotlinlang.org/docs/lambdas.html#standard-functions)
- [MDN Web Docs: String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)