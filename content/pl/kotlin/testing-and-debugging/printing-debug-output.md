---
title:                "Drukowanie komunikatów debugowania"
date:                  2024-01-20T17:52:57.652041-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Drukowanie danych debugowania to jak rozmowa z programem: "Co robisz?". Programiści używają tego do śledzenia wartości i zachowania aplikacji podczas pisania kodu - znacznie ułatwia to znalezienie i naprawienie błędów.

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
