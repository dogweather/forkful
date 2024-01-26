---
title:                "Pisanie do standardowego błędu"
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?
Standard Error, znany jako `stderr`, to strumień do zgłaszania błędów i komunikatów diagnostycznych. Programiści używają go do oddzielania normalnego outputu od błędów, co ułatwia logowanie i debugowanie.

## Jak to zrobić:
W Kotlinie piszemy do `stderr` bezpośrednio przez `System.err`, podobnie jak do `stdout` przez `System.out`.

```kotlin
fun main() {
    println("To jest do stdout") // Normalny output
    System.err.println("To jest do stderr") // Błąd / komunikat diagnostyczny
}
```

Wynik:
```
To jest do stdout
To jest do stderr
```
*Wyniki możemy zobaczyć w oddzielnych strumieniach w konsoli.*

## Deep Dive
Historycznie, separacja `stdout` od `stderr` pochodzi z czasów Uniksa i jest używana do dziś w większości systemów operacyjnych. Mimo że w Kotlinie najczęściej używa się `println` dla prostego outputu, `System.err` jest dedykowany dla błędów. W przeciwieństwie do `stdout`, `stderr` jest domyślnie niebuforowany, więc komunikaty pojawiają się od razu bez opóźnienia.

Alternatywą dla pisania do `stderr` może być własny system logowania lub zewnętrzne biblioteki jak `logback` czy `log4j`, które oferują większą kontrolę i elastyczność.

Co do implementacji, Kotlin (oparty na Javie) przekierowuje wywołania `System.err.println()` do natywnego strumienia `stderr` definowanego przez środowisko uruchomieniowe.

## See Also
- Dokumentacja Kotlin-a: [kotlinlang.org/docs/reference](https://kotlinlang.org/docs/reference)
- Szczegółowy opis `System.err`: [Oracle Java Docs](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- Porównanie systemów logowania w Kotlinie: [Baeldung on Logging in Kotlin](https://www.baeldung.com/kotlin/logging)
