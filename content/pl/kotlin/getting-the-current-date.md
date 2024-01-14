---
title:                "Kotlin: Pobieranie aktualnej daty"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego warto pobrać bieżącą datę?

Pobieranie bieżącej daty jest niezwykle przydatne w wielu rodzajach projektów. Może służyć do monitorowania wydarzeń, tworzenia logów, czy też tylko jako informacja dla użytkownika o aktualnym czasie. W niniejszym artykule dowiesz się, jak w prosty sposób w Kotlinie pobrać bieżącą datę oraz jakie są najczęściej wykorzystywane metody.

## Jak to zrobić?

Aby pobrać bieżącą datę w Kotlinie, potrzebujemy jedynie jednej linijki kodu:

```Kotlin
val currentDate = Date()
```

W powyższym przykładzie tworzymy obiekt typu Date, który przechowuje informację o aktualnym czasie. Następnie możemy wykorzystać różne metody na tym obiekcie, np.:

```Kotlin
currentDate.year // zwraca rok
currentDate.month // zwraca miesiąc
currentDate.day // zwraca dzień
currentDate.hours // zwraca godzinę
currentDate.minutes // zwraca minutę
currentDate.seconds // zwraca sekundę
```

Możemy również wyświetlić całą datę w czytelnej formie, wykorzystując metodę `toString()`:

```Kotlin
println(currentDate.toString())
```

Powyższy kod wyświetli bieżącą datę w formacie `Tue Apr 13 17:21:35 CEST 2021`.

## Głębszy zanurzenie w temat

Istnieją również inne sposoby na pobranie aktualnej daty w Kotlinie, takie jak wykorzystanie klasy `Calendar` lub pakietu `java.time`. Jednak przy wykorzystaniu klasy `Date` musimy pamiętać, że nie jest ona bezpieczna w wielowątkowym środowisku. Zaleca się zatem wykorzystywanie klasy `Instant` lub `LocalDateTime` z pakietu `java.time`.

## Zobacz także

- Dokumentacja klasy Date w bibliotece standardowej Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/
- Przewodnik po Java.time API: https://www.baeldung.com/java-8-date-time-intro 
- Podstawy programowania w Kotlinie: https://kotlinlang.org/docs/getting-started.html