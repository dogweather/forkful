---
date: 2024-01-20 17:31:19.992591-07:00
description: "Hur g\xF6r man: Exempelutdatan kan se ut s\xE5 h\xE4r."
lastmod: '2024-04-05T21:53:39.221429-06:00'
model: gpt-4-1106-preview
summary: "Exempelutdatan kan se ut s\xE5 h\xE4r."
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
weight: 26
---

## Hur gör man:
```kotlin
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun main() {
    val today = LocalDate.now()
    val tenDaysLater = today.plusDays(10)
    val thirtyDaysAgo = today.minusDays(30)

    println("Idag: $today")
    println("Om tio dagar: $tenDaysLater")
    println("Trettio dagar sedan: $thirtyDaysAgo")
}
```
Exempelutdatan kan se ut så här:
```
Idag: 2023-04-03
Om tio dagar: 2023-04-13
Trettio dagar sedan: 2023-03-04
```

## Fördjupning
I Java världen handskades datum manipulation länge med `java.util.Date` och `java.util.Calendar`, men de var klumpiga och inte trådsäkra. Java 8 introducerade `java.time` (JSR-310), som gav oss en robust och lättanvänd API för datum och tid, och Kotlin har adopterat detta. Alternativ till den inbyggda API:n inkluderar Joda-Time, även om dess användning är mindre vanlig sedan `java.time` blev standard. När du arbetar med datum och tider, oavsett om du använder inbyggda funktioner eller bibliotek, är det viktigt att överväga tidszoner och iaktta bästa praxis för datumperiodsberäkningar, såsom att undvika hårdkodade värden (ex. "30 dagar per månad") på grund av varierande månadslängder och skottår.

## Se även
- [Kotlin-dokumentation för datum och tid](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/)
- [java.time paketet](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Video om datum och tid i Kotlin](https://www.youtube.com/watch?v=Ix8qz9anQdo)
