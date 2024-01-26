---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means changing the first letter of each word to uppercase. Programmers do it to ensure names, titles, and other important elements are formatted correctly and are easily readable.
 
## How to:
In Kotlin, you can capitalize a string using `replaceFirstChar` and `uppercase`.

```Kotlin
fun main() {
    val lowercaseString = "kotlin jest fajny"
    val capitalizedString = lowercaseString.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }

    println(capitalizedString) // Output: Kotlin jest fajny
}
```

## Deep Dive
Before Kotlin 1.5, developers commonly used `capitalize()` for this purpose. However, it was deprecated because it only worked with the English alphabet correctly. Now, `replaceFirstChar` combined with `titlecase()` or `uppercase` covers more languages respecting Unicode standards.

An alternative approach would use the `split` function to create a list, capitalize each element, and then `joinToString`:

```Kotlin
fun String.capitalizeWords(): String =
    split(" ").joinToString(" ") { it.replaceFirstChar { char -> char.uppercase() } }

fun main() {
    val title = "pan tadeusz"
    val capitalizedTitle = title.capitalizeWords()

    println(capitalizedTitle) // Output: Pan Tadeusz
}
```

This method capitalizes the first letter of each word, not just the first letter of the string.

## See Also
- Kotlin Standard Library Documentation: https://kotlinlang.org/api/latest/jvm/stdlib/
- Unicode Standard: http://www.unicode.org/standard/standard.html
- Practical applications of string capitalization: https://developer.android.com/guide/topics/resources/string-resource#FormattingAndStyling
