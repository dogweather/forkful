---
title:                "Porównywanie dwóch dat"
date:                  2024-01-20T17:33:48.437328-07:00
model:                 gpt-4-1106-preview
simple_title:         "Porównywanie dwóch dat"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Porównywanie dwóch dat polega na ocenie, która z nich jest wcześniejsza, późniejsza lub czy są identyczne. Programiści robią to, aby zarządzać terminami, ustalać priorytety i analizować okresy czasu w aplikacjach.

## Jak to zrobić?

```kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 4, 1)
    val date2 = LocalDate.of(2023, 5, 1)

    println(date1.isBefore(date2))  // Wynik: true
    println(date1.isAfter(date2))   // Wynik: false
    println(date1.isEqual(date2))   // Wynik: false
}
```
Kod wykorzystuje `LocalDate` z pakietu `java.time` i trzy metody: `isBefore()`, `isAfter()` oraz `isEqual()`, by określić stosunek dat do siebie.

## Głębsze spojrzenie

Porównywanie dat nie zawsze było takie proste. W przeszłości programiści musieli polegać na bibliotekach zewnętrznych lub własnych algorytmach. Java 8 wprowadziła pakiet `java.time`, oferując uproszczenia, w tym `LocalDate`. Alternatywy jak Joda-Time są nadal popularne, lecz nie zawsze potrzebne z nowym API.

Zaimplementowanie porównywania dat obejmuje wiele przypadków — strefy czasowe, lata przestępne itp. `LocalDate` porównuje wyłącznie daty, nie czas i strefę, co może być zaletą lub ograniczeniem, w zależności od kontekstu.

## Zobacz też

- Oficjalna dokumentacja `LocalDate`: [LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- Blog na temat nowego API daty i czasu w Javie: [Modern Java - A Guide to Java 8](https://www.baeldung.com/java-8-date-time-intro)
- Porównanie Joda-Time i `java.time`: [Joda-Time vs. java.time](https://www.baeldung.com/joda-time)
