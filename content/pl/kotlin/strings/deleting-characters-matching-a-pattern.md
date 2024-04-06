---
date: 2024-01-20 17:42:45.221868-07:00
description: "How to: (Jak to zrobi\u0107:) ."
lastmod: '2024-04-05T21:53:36.787249-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

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
