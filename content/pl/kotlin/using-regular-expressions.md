---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyrażenia regularne (regex), to narzędzie do szukania i manipulacji tekstami. Programiści używają ich dla szybkości i elastyczności w przetwarzaniu wzorców w tekście.

## Jak to zrobić:

```kotlin
fun main() {
    val text = "To jest przykładowy tekst z numerem telefonu 123-456-7890."
    val regex = "\\d{3}-\\d{3}-\\d{4}".toRegex()

    // Wyszukiwanie pasujących ciągów
    val found = regex.find(text)
    println(found?.value) // Output: 123-456-7890

    // Zastępowanie pasujących ciągów
    val replaced = text.replace(regex, "###-###-####")
    println(replaced) // Output: To jest przykładowy tekst z numerem telefonu ###-###-####.

    // Sprawdzanie dopasowania
    val isMatch = regex.matches("123-456-7890")
    println(isMatch) // Output: true
}
```

## Deep Dive

Wyrażenia regularne mają korzenie w teorii automatów i formalnych języków—są stworzone w latach 50. Istnieją alternatywy jak `String.contains`, `String.startsWith` dla prostych przypadków. Kotlin używa silnika regex Javy, ale udostępnia własne API, aby ułatwić pracę.

## See Also

- Dokumentacja Kotlin Regex: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- Tutorial Java Regex dla głębszego zrozumienia: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html
- Projekt "regular expressions" w teorii obliczeń: https://www.cs.rochester.edu/~nelson/courses/csc_173/computation/regex.html
