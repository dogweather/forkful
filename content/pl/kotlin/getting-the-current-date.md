---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:15:47.216633-07:00
simple_title:         "Pobieranie aktualnej daty"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
W Kotlinie pobieranie bieżącej daty to kwestia wykorzystania odpowiednich bibliotek do ustalenia aktualnego momentu. Programiści robią to, żeby rejestrować zdarzenia, liczyć czas albo po prostu wyświetlać datę użytkownikom.

## Jak to zrobić:
```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Dzisiaj jest: $today")
}

// Przykładowe wyjście:
// Dzisiaj jest: 2023-03-15
```

## W głębinach tematu:
Historia tej kwestii sięga czasów Javy, gdzie pierwotnie używano typu `java.util.Date`, ale był on problematyczny z wielu względów, między innymi przez nieuwzględnianie stref czasowych i mutowalność. Od Javy 8 wprowadzono nowe API daty i czasu z `java.time`, które Kotlin dziedziczy, zapewniając niezmienność i lepszą obsługę stref czasowych.

Alternatywą jest użycie biblioteki Joda-Time, która była popularna przed java.time, ale teraz jest już rzadziej stosowana z powodu lepszego wsparcia, jakie oferuje standardowe API.

Szczegóły implementacyjne rozwiązania opierają się głównie na wykorzystaniu klasy `LocalDate` do reprezentacji daty bez czasu oraz `LocalDateTime` do reprezentacji daty z czasem, a także `ZonedDateTime` do pracy ze strefami czasowymi.

## Zobacz również:
- Porównanie Joda-Time i java.time [Joda-Time vs java.time](http://blog.joda.org/2014/11/converting-from-joda-time-to-javatime.html)
- Dokumentacja Oracle dla java.time [Oracle's java.time tutorial](https://docs.oracle.com/javase/tutorial/datetime/)
