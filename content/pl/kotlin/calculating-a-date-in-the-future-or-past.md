---
title:                "Kotlin: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Liczenie daty w przyszłości lub przeszłości może być przydatne w różnych sytuacjach, na przykład w tworzeniu aplikacji lub skryptów, które wymagają uwzględnienia różnych dat. Odczytanie lub ustalenie daty za pomocą kodu może także być przydatne w celu uproszczenia procesów.

## Jak to zrobić

W celu obliczenia daty w przyszłości lub przeszłości w języku Kotlin, można użyć klasy `Calendar`, `LocalDate` lub `LocalDateTime`. W poniższych przykładach użyto klasy `Calendar`.

```Kotlin
    // Dla daty za 7 dni
    val calendar = Calendar.getInstance()
    calendar.add(Calendar.DATE, 7)
    val futureDate = calendar.time

    // Dla daty za 2 tygodnie
    val calendar = Calendar.getInstance()
    calendar.add(Calendar.WEEK_OF_YEAR, 2)
    val futureDate = calendar.time

    // Dla daty za 3 miesiące
    val calendar = Calendar.getInstance()
    calendar.add(Calendar.MONTH, 3)
    val futureDate = calendar.time

    // Dla daty w przeszłości, np. 5 lat temu
    val calendar = Calendar.getInstance()
    calendar.add(Calendar.YEAR, -5)
    val pastDate = calendar.time
```

Output:
```Kotlin
    // Dla daty za 7 dni
    Mon Jun 21 16:18:45 CEST 2021

    // Dla daty za 2 tygodnie
    Sat Jul 03 16:18:45 CEST 2021

    // Dla daty za 3 miesiące
    Sat Sep 11 16:18:45 CEST 2021

    // Dla daty w przeszłości, np. 5 lat temu
    Thu Jun 04 16:18:45 CEST 2015
```

## Głębszy wgląd

Klasy `Calendar`, `LocalDate` i `LocalDateTime` oferują wiele metod do manipulowania datami, takich jak `add()`, `subtract()`, `isLeapYear()` czy `get()`. Jest to szczególnie przydatne w bardziej zaawansowanych operacjach związanych z datami.

Ponadto, w celu dokładnego zrozumienia działania tych instrukcji, ważne jest zrozumienie pojęć takich jak strefy czasowe i formaty dat, które mogą mieć wpływ na wynik działania.

## Zobacz również

- <https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html>
- <https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date-time.html>
- <https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date.html>

Dzięki użyciu wiedzy z tego artykułu, można sprawnie manipulować datami w aplikacjach Kotlin. Pamiętaj, aby dokładnie zapoznać się z dokumentacją i eksperymentować z różnymi metodami, aby lepiej zrozumieć ich działanie.