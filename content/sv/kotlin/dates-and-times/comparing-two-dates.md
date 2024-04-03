---
date: 2024-01-20 17:33:30.575466-07:00
description: "How to: Kotlin g\xF6r det enkelt att j\xE4mf\xF6ra datum med `LocalDate`\
  \ klassen. S\xE5 h\xE4r g\xF6r du."
lastmod: '2024-03-13T22:44:37.883571-06:00'
model: gpt-4-1106-preview
summary: "Kotlin g\xF6r det enkelt att j\xE4mf\xF6ra datum med `LocalDate` klassen."
title: "J\xE4mf\xF6ra tv\xE5 datum"
weight: 27
---

## How to:
Kotlin gör det enkelt att jämföra datum med `LocalDate` klassen. Så här gör du:

```kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 4, 1)
    val date2 = LocalDate.now()

    println(date1.isBefore(date2))  // True if date1 is before date2
    println(date1.isAfter(date2))   // True if date1 is after date2
    println(date1.isEqual(date2))   // True if date1 is equal to date2
}
```
Kör koden och output blir baserat på datumet när du kör den.

## Deep Dive
Kotlin använder `java.time` paketet introducerat i Java 8 för datumhantering, som var en stor förbättring jämfört med de tidigare `Date` och `Calendar`. Alternativt kan man använda tredjepartsbibliotek som Joda-Time, men sedan `java.time` finns behöver man sällan göra det. Implementationen av datumjämförelser använder interna klockor och tidszoner, så överväg detta när du hanterar datum globalt.

## See Also
- Kotlin officiella dokumentation: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- java.time.LocalDate API doc: [https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- Att hantera tidszoner: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
