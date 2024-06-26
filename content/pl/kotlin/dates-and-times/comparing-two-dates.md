---
date: 2024-01-20 17:33:48.437328-07:00
description: "Jak to zrobi\u0107? Kod wykorzystuje `LocalDate` z pakietu `java.time`\
  \ i trzy metody: `isBefore()`, `isAfter()` oraz `isEqual()`, by okre\u015Bli\u0107\
  \ stosunek dat do\u2026"
lastmod: '2024-04-05T21:53:36.814708-06:00'
model: gpt-4-1106-preview
summary: Kod wykorzystuje `LocalDate` z pakietu `java.time` i trzy metody.
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

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
