---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means turning the first letter of each word to uppercase. Programmers do it to format text, ensuring names, titles, or UI elements look neat and standardized.

## How to:

In Kotlin, you can capitalize strings easily. Here's a quick example:

```kotlin
fun main() {
    val text = "kotlin programming"
    val capitalizedText = text.split(" ").joinToString(" ") { it.capitalize() }
    println(capitalizedText)
}
```

Sample Output:
```
Kotlin Programming
```
To capitalize just the first letter of a sentence:

```kotlin
fun main() {
    val sentence = "hello, kotlin enthusiasts!"
    val capitalizedSentence = sentence.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }
    println(capitalizedSentence)
}

```

Sample Output:
```
Hello, kotlin enthusiasts!
```

Note that `capitalize()` is deprecated. Use `replaceFirstChar { it.titlecase() }` for better future compatibility.

## Deep Dive

Capitalization methods changed in Kotlin. `capitalize()` was used broadly but got deprecated in favor of `replaceFirstChar { it.titlecase() }`. This change makes code clearer about what’s happening—it's not just capitalizing but replacing the first character with its titlecase equivalent.

Why capitalize strings? It's often a user interface thing. Think book titles, names, or any list where you need consistency. It helps with readability and aesthetics.

Alternatives to capitalizing include:
- `.toLowerCase()`: For lowercasing.
- `.toUpperCase()`: For making everything uppercase.
- CSS in web development: sometimes text is capitalized in the frontend.

Under the hood, capitalization functions interact with Unicode characters. Characters have specific uppercase versions. It's not just about slapping an 'A' where there was an 'a', it's about understanding language-specific rules.

Don't forget about locales. In Turkish, for instance, 'i' capitalizes to 'İ', not 'I'. So, doing it locale-agnostic might trip you up in multi-language applications.

## See Also:

- Kotlin docs on `replaceFirstChar`: [Kotlin replaceFirstChar](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace-first-char.html)
- Unicode capitalization rules: [Unicode Capitalization Guidelines](http://unicode.org/versions/Unicode9.0.0/ch03.pdf#G33992)
- Capitalization in different locales: [Locale-Specific Capitalization](https://garygregory.wordpress.com/2015/11/03/java-lowercase-conversion-turkey/)
