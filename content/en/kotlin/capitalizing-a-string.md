---
title:                "Capitalizing a string"
html_title:           "Kotlin recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means converting the first letter of each word in the string to uppercase, while leaving the rest of the letters unchanged. This is a common practice in programming to improve the readability and consistency of text. It is especially useful for titles, names, and display purposes.

## How to:

To capitalize a string in Kotlin, we can use the `capitalize()` function. This function takes no parameters and returns a new string with the first letter capitalized. Here's an example:

```Kotlin
val text = "hello world"
val capitalizedText = text.capitalize()
// Output: "Hello world"
```

We can also use the `capitalizeWords()` function from the `kotlin.text` package to capitalize each word in a string. This function takes a `Locale` parameter to specify the language rules for capitalization. Here's an example:

```Kotlin
val text = "hello world"
val capitalizedWords = text.capitalizeWords(Locale.ENGLISH)
// Output: "Hello World"
```

## Deep Dive:

Capitalizing strings has been a common practice in the English language for centuries, with rules and styles varying across different languages and cultures. In programming, capitalizing strings is not only aesthetically pleasing, but also improves the consistency and legibility of code. It also helps in avoiding confusion and errors caused by inconsistent capitalization.

An alternative to capitalizing strings is using the `toUpperCase()` function, which converts all letters in a string to uppercase. However, this is not suitable for cases where only the first letter needs to be capitalized.

In Kotlin, the `capitalize()` function uses `String.toCharArray()` and `Character.toUpperCase()` methods to manipulate the string. This function was introduced in Kotlin 1.4 and is available for use in all platforms - JVM, JS, and Native.

## See Also:

- [Kotlin Docs on Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin String API Reference](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/)
- [Wikipedia on Capitalization](https://en.wikipedia.org/wiki/Capitalization)