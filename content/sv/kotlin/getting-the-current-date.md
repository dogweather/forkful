---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:15:37.908982-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet innebär att få tag i dagens datum. Programmerare gör det för att logga händelser, spåra användaraktivitet eller hantera datumrelaterade funktioner.

## Så Här Gör Du:
För att få det aktuella datumet i Kotlin använder vi `LocalDate.now()`. Här är en simpel snutt som visar hur det går till:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Idag är det: $today")
}

// Exempel på utskrift: Idag är det: 2023-04-05
```

Låt oss även visa tid med `LocalDateTime.now()`:

```kotlin
import java.time.LocalDateTime

fun main() {
    val now = LocalDateTime.now()
    println("Just nu är klockan: $now")
}

// Exempel på utskrift: Just nu är klockan: 2023-04-05T21:45:00.123
```

Observera att `LocalDateTime` inkluderar både datum och tid.

## Djupdykning
I historisk mening har datum- och tidshantering gjorts på många olika sätt i Java-världen. `java.util.Date` var länge standard men hade många problem, såsom dålig API-design och brist på tidszonsstöd.

Kotlin bygger på JVM och därmed kan använda Java's moderna `java.time`-bibliotek introducerat i Java 8. Det ger en robust och tidszonsmedveten hantering av datum och tider. `LocalDate` och `LocalDateTime` är två centrala klasser i detta bibliotek. 

Alternativet till `java.time` är `java.util.Calendar`, men det rekommenderas inte längre på grund av dess komplexitet och mindre intuitiva API.

Bakom kulisserna använder `LocalDate.now()` och `LocalDateTime.now()` systemklockan för att hämta aktuell information, vilket vi kan anse vara ganska rakt på sak när det handlar om att förstå vad "nu" är.

## Se Även
- [JVM-dokumentation för java.time.LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [JVM-dokumentation för java.time.LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
