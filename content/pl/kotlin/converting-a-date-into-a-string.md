---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja daty na łańcuch znaków to proces przekształcania formatu daty do czytelnej formy tekstu. Programiści robią to głównie, aby łatwiej zaprezentować datę użytkownikowi w wybranym formacie.

## Jak to zrobić:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    val today = LocalDate.now()
    val formattedDate = today.format(dateFormatter)
    println(formattedDate)
}
```

Wyjście:
```Kotlin
19.02.2023
```
W powyższym kodzie tworzymy obiekt formatu daty, a następnie przekształcamy dzisiejszą datę na łańcuch znaków za pomocą metody format.

## Deep Dive:

1. Kontekst historyczny: Formatowanie dat jest konieczne od początków programowania komputerowego. Ułatwia to zarówno użytkownikom, jak i programistom zarządzanie informacjami związanymi z datami.
   
2. Alternatywy: Można również skorzystać z metody SimpleDateFormat w Javie, jednak jest ona mniej bezpieczna i bardziej podatna na błędy.
   
3. Szczegóły implementacji: Metoda format jest częścią biblioteki Java Time, która jest integralną częścią Kotlin od wersji 1.0. Ta biblioteka oferuje wiele różnych szablonów do formatowania dat, jak również możliwość tworzenia własnych szablonów.

## Zobacz również:

1. Dokumentacja Kotlin do formatowania dat: [link](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date-time-formatter/index.html)
2. Dokumentacja Java Time: [link](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
3. Przewodnik po różnych szablonach formatowania dat: [link](https://www.ibm.com/docs/en/i/7.2?topic=concepts-date-time-format)