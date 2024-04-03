---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:37.263898-07:00
description: "Jak to zrobi\u0107: Kotlin wspiera parsowanie dat za pomoc\u0105 pakietu\
  \ `java.time`, wprowadzonego w Java 8. Oto prosty spos\xF3b u\u017Cycia `LocalDateTime`\
  \ i konkretnego\u2026"
lastmod: '2024-03-13T22:44:35.374388-06:00'
model: gpt-4-0125-preview
summary: "Kotlin wspiera parsowanie dat za pomoc\u0105 pakietu `java.time`, wprowadzonego\
  \ w Java 8."
title: "Analiza sk\u0142adniowa daty z \u0142a\u0144cucha znak\xF3w"
weight: 30
---

## Jak to zrobić:
Kotlin wspiera parsowanie dat za pomocą pakietu `java.time`, wprowadzonego w Java 8. Oto prosty sposób użycia `LocalDateTime` i konkretnego wzorca:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val data = parseDateFromString(dateString)
    println(data)  // Wynik: 2023-04-01T12:00
}
```

Dla większej elastyczności, lub aby obsłużyć daty z zewnętrznych źródeł, takich jak API, można użyć biblioteki stron trzecich, takiej jak Joda-Time (choć jest to mniej powszechne teraz, gdy `java.time` jest solidne). Jednakże, przestrzeganie nowoczesnego podejścia dostarczonego przez JDK jest preferowane dla większości aplikacji Kotlin.

Aby parsować daty w Kotlinie bez użycia bibliotek stron trzecich, można również skorzystać z klasy `SimpleDateFormat` dla wersji przed Java 8 lub poziomów API Androida, które nie obsługują `java.time`:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val data = parseDateUsingSimpleDateFormat(dateString)
    println(data)  // Wynik będzie się różnił w zależności od strefy czasowej, np., Sob Apr 01 12:00:00 GMT 2023
}
```

Pamiętaj, aby zawsze ustawić strefę czasową przy pracy z `SimpleDateFormat`, aby uniknąć nieoczekiwanych przesunięć w parsowanych datach.
