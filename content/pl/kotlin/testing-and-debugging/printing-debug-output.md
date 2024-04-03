---
date: 2024-01-20 17:52:57.652041-07:00
description: "Drukowanie danych debugowania to jak rozmowa z programem: \"Co robisz?\"\
  . Programi\u015Bci u\u017Cywaj\u0105 tego do \u015Bledzenia warto\u015Bci i zachowania\
  \ aplikacji podczas\u2026"
lastmod: '2024-03-13T22:44:35.367474-06:00'
model: gpt-4-1106-preview
summary: 'Drukowanie danych debugowania to jak rozmowa z programem: "Co robisz.'
title: "Drukowanie komunikat\xF3w debugowania"
weight: 33
---

## How to: (Jak to zrobić:)
W Kotlinie wypisywanie debugowania jest banalne. Używasz `println()` do wyrzucania wartości na konsolę.

```kotlin
fun main() {
    val interestingValue = 42
    println("Interesting value is $interestingValue")
}
```
Output:
```
Interesting value is 42
```
Opcjonalnie, dla zmiennych i wyrażeń użyj `$` wewnątrz stringów.

## Deep Dive (Głębsze zanurzenie)
Kiedyś, w erze komputerów kart perforowanych, debugowanie było jak szukanie igły w stogu siana. Dzisiaj `println()` jest tylko wierzchołkiem góry lodowej opcji debugowania. Alternatywy to loggery takie jak Log4j, które oferują poziomy logowania (np. INFO, DEBUG, ERROR). Są też IDE z wbudowanymi debuggerami pokazującymi co jest grane na żywo. Implementacja `println()` jest prosta i bezpośrednio pisze do standardowego wyjścia (stdout), a to oznacza konsolę dla większości programów.

## See Also (Zobacz również)
- [Dokumentacja Kotlin – Logging](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- [Przewodnik po loggerach w Kotlinie](https://www.baeldung.com/kotlin/logging)
