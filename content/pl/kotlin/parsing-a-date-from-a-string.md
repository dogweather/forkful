---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:37:16.671886-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie daty z łańcucha znaków to zamiana tekstu na obiekt daty. Umożliwia to manipulację datą i porównywanie z innymi datami w programie.

## Jak to zrobić:
```kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-04-05"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val parsedDate = LocalDate.parse(dateString, formatter)
    
    println("Parsed date: $parsedDate")
}
```
Wyjście:
```
Parsed date: 2023-04-05
```

## Wnikliwe spojrzenie
Parsowanie dat w Kotlinie opiera się głównie na klasach i metodach z pakietu `java.time`, wprowadzonego w Javie 8. Alternatywą jest użycie `SimpleDateFormat` z `java.text`, ale to starsze rozwiązanie jest mniej bezpieczne. Ważną kwestią jest używanie odpowiedniego wzorca daty, np. `yyyy-MM-dd` dla ISO. Pamiętaj też o różnych strefach czasowych i lokalizacjach.

## Zobacz także
- [Dokumentacja klasy LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Dokumentacja klasy DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Poradnik Oracle do `java.time`](https://docs.oracle.com/javase/tutorial/datetime/iso/format.html)
- [Różnice między `java.time` a `SimpleDateFormat`](https://www.baeldung.com/java-8-date-time-intro)
