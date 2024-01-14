---
title:                "Kotlin recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string is a common task in programming, especially when dealing with user input or data from external sources. It involves changing the first letter of a sentence or word from lowercase to uppercase, following proper capitalization rules.

## How To

To capitalize a string in Kotlin, we first need to create a function that takes in a string as a parameter. Within this function, we will use a built-in method called `capitalize()` to capitalize the first letter of the string.

```
Kotlin
fun capitalizeString(str: String): String {
    return str.capitalize()
}

println(capitalizeString("kotlin is awesome"))

Output: Kotlin is awesome
```

We can also convert an entire sentence by splitting it into a list of words, capitalizing the first letter of each word, and then joining the list back into a string.

```
Kotlin
fun capitalizeString(str: String): String {
    val words = str.split(" ")
    val capitalizedWords = words.map { it.capitalize() }
    return capitalizedWords.joinToString(" ")
}

println(capitalizeString("kotlin is awesome"))

Output: Kotlin Is Awesome
```

## Deep Dive

The `capitalize()` method in Kotlin uses the rules of the English language, where the first letter after a period, question mark, or exclamation mark is automatically capitalized. It also takes into consideration words that are already capitalized, such as proper nouns, and leaves them as is.

However, it is important to note that this method may not work correctly for all languages or specific cases. For example, words with accent marks or non-English characters may not be capitalized correctly. In such cases, it may be necessary to create a custom function or use a library specifically designed for the language or use case.

## See Also

If you want to dive deeper into capitalizing strings in Kotlin, check out the official Kotlin documentation on String Manipulation and explore other methods and functions that can help with formatting and modifying strings.

- [Official Kotlin Documentation for String Manipulation](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)
- [Kotlin Standard Library - String Functions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/)
- [Kotlin String Capitalization Example](https://www.baeldung.com/kotlin/string-capitalization)