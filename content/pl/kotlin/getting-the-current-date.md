---
title:                "Pobieranie aktualnej daty"
html_title:           "Kotlin: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Pobieranie aktualnej daty jest jednym z podstawowych zadań, które często wykonują programiści. Jest to niezbędne dla wielu aplikacji, które muszą wyświetlać aktualną datę lub ją analizować.

## Jak to zrobić?

Możemy łatwo pobrać aktualną datę w języku Kotlin za pomocą wbudowanej funkcji `LocalDate.now()`. Poniższy przykład pokazuje, jak można wyświetlić aktualną datę w formacie "dd/MM/yyyy":

```Kotlin
val currentDate = LocalDate.now()
println(currentDate.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))) // Output: 15/11/2021
```

Możemy również pobrać aktualny miesiąc lub rok z daty za pomocą odpowiednich funkcji `month` oraz `year`.

```Kotlin
// Pobieranie aktualnego miesiąca
val currentMonth = currentDate.month
println(currentMonth) // Output: NOVEMBER

// Pobieranie aktualnego roku
val currentYear = currentDate.year
println(currentYear) // Output: 2021
```

## Głębszy zanurzenie

Funkcja `LocalDate.now()` została wprowadzona w JDK 8 i jest częścią biblioteki Java Time API. Dzięki temu możemy wygodnie i precyzyjnie manipulować datami i czasem w języku Kotlin.

Alternatywnym sposobem na pobranie aktualnej daty jest użycie klasy `Calendar`. Jednak Java Time API jest zalecanym sposobem, ponieważ jest lepiej zaprojektowane i bardziej intuicyjne.

Implementacja funkcji `LocalDate.now()` korzysta z systemowego zegara, który przechowuje aktualną datę i czas. Dzięki temu można uniknąć błędów związanych z różnicami w strefie czasowej w różnych regionach.

## Zobacz również

- Dokumentacja Java Time API: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Przewodnik dla języka Kotlin: https://kotlinlang.org/docs/datetime.html