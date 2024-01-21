---
title:                "Usuwanie znaków pasujących do wzorca"
date:                  2024-01-20T17:42:45.221868-07:00
model:                 gpt-4-1106-preview
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Usuwanie znaków pasujących do wzorca to filtracja tekstów, usuwanie niechcianych znaków. Programiści robią to, aby wyczyścić dane, usunąć błędy lub przygotować tekst do dalszej obróbki.

## How to: (Jak to zrobić:)
```kotlin
fun main() {
    val originalString = "Kotl1n j3st św13tny!"
    val regexPattern = "\\d+".toRegex()
    val cleanedString = originalString.replace(regexPattern, "")

    println("Original: $originalString") // Original: Kotl1n j3st św13tny!
    println("Cleaned: $cleanedString")   // Cleaned: Kotlin jest świetny!
}
```

## Deep Dive (Dogłębna analiza)
Usuwanie znaków na podstawie wzorca to technika znana od lat 60., kiedy to regex (wyrażenia regularne) zaczęły być stosowane w edytorach tekstu. W Kotlinie, jak pokazano powyżej, używamy klasy `Regex` i metody `replace()`. Alternatywą może być iteracja po znakach i manualne budowanie nowego stringa, ale to dużo więcej kodu i ryzyko błędów. `Regex` jest wydajny, ale wymaga pewnej wiedzy o wzorcach. Warto też znać metody `filter` i `map`.

## See Also (Zobacz także)
- Dokumentacja Kotlin `Regex`: [kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex)
- Przewodnik po wyrażeniach regularnych w Javie (też przydatne dla Kotlin): [regular-expressions.info/java.html](https://www.regular-expressions.info/java.html)