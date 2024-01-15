---
title:                "Uzyskiwanie aktualnej daty"
html_title:           "Kotlin: Uzyskiwanie aktualnej daty"
simple_title:         "Uzyskiwanie aktualnej daty"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego?

Dokładne śledzenie czasu jest ważne w wielu projektach programistycznych, a wiele z nich wymaga aktualnej daty. Dzięki temu artykułowi dowiesz się, jak w łatwy sposób uzyskać aktualną datę w języku Kotlin.

## Jak to zrobić?

```Kotlin
val currentDate = LocalDate.now()
println(currentDate)
```

Ten prosty kod utworzy zmienną zawierającą aktualną datę i wyświetli ją w konsoli. Jest to możliwe dzięki klasie `LocalDate` z pakietu `java.time`.

Możesz również pobrać aktualną datę w innych strefach czasowych, używając metody `now (zone: ZoneId)` zamiast `now ()`. Na przykład:

```Kotlin
val currentDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
println(currentDate)
```

To spowoduje pobranie aktualnej daty w strefie czasowej Europy/Warszawy.

## Deep Dive

Jeśli chcesz uzyskać więcej informacji na temat czasu, możesz użyć klasy `LocalDateTime`. Ta klasa pozwala na uzyskanie aktualnego czasu wraz z datą. Na przykład:

```Kotlin
val currentDateTime = LocalDateTime.now()
println(currentDateTime)
```

Otrzymujesz wynik w formacie `yyyy-MM-ddTHH:mm:ss.mmm`, gdzie `T` oznacza separator pomiędzy datą a czasem, a `mmm` to milisekundy.

Dodatkowo, jeśli potrzebujesz aktualnego czasu w konkretnym formacie, możesz użyć metody `format(DateTimeFormatter)`. Na przykład, aby wyświetlić datę w formacie dd-MM-yyyy, możesz użyć następującego kodu:

```Kotlin
val currentDate = LocalDate.now()
println(currentDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy")))
```

Ten kod powinien zwrócić aktualną datę w postaci dd-MM-yyyy.

## Zobacz także

- [Dokumentacja pakietu java.time](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/package-summary.html)
- [Tutorial Kursu Kotlin - Daty i Czasy](https://kotlinlang.org/docs/datetime.html)
- [Przewodnik po pakiecie java.time](https://www.baeldung.com/java-8-date-time-intro)