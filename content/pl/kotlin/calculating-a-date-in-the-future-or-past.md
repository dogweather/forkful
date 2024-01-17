---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Kotlin: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co to jest i po co to robić?

Obliczanie daty w przeszłości lub przyszłości jest rutynowym zadaniem, które programiści muszą wykonywać w wielu projektach. Pliki, bazy danych, a nawet interfejsy użytkownika często wymagają określenia daty, która jest wcześniejsza lub późniejsza od bieżącej. Dlatego znajomość tego procesu jest koniecznością dla każdego programisty.

## Jak to zrobić?

Aby obliczyć datę w przeszłości lub przyszłości w języku Kotlin, wystarczy skorzystać z metody `plus` lub `minus` dostępnej na obiekcie `LocalDate`. Należy również podać liczbę dni, miesięcy lub lat, które chcemy dodać lub odjąć od bieżącej daty.

```Kotlin
val currentDate = LocalDate.now()
val futureDate = currentDate.plusDays(10)
val pastDate = currentDate.minusMonths(2)
```

W tym przykładzie utworzyliśmy trzy zmienne: `currentDate` przechowującą bieżącą datę, `futureDate` z datą 10 dni później i `pastDate` z datą 2 miesiące wcześniej. Dzięki prostocie języka Kotlin, obliczanie dat w przeszłości lub przyszłości jest bardzo wygodne i intuicyjne.

## Zagłębienie się

Obliczanie dat w przeszłości lub przyszłości jest standardowym procesem w wielu językach programowania. Jednak w języku Kotlin jest to jeszcze prostsze dzięki wykorzystaniu metody `plus` i `minus` na obiekcie `LocalDate`. Istnieją również inne sposoby na wykonanie tego zadania, na przykład korzystając z biblioteki Joda-Time lub wykorzystując obiekt `Calendar` w języku Java. Jednak metoda Kotlin jest bardziej czytelna i wygodniejsza w użyciu.

## Zobacz również

[Dokumentacja Kotlin o dacie i czasie](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/)

[Kurs Kotlin na Courserze - Manipulacja datami i czasem](https://www.coursera.org/learn/kotlin-for-java-developers/lecture/GIWUV/dates-and-times-in-kotlin)