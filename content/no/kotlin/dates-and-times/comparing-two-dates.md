---
date: 2024-01-20 17:33:15.856159-07:00
description: "Sammenligning av to datoer handler om \xE5 finne ut om de er like, eller\
  \ hvilken som kommer f\xF8r eller etter den andre. Utviklere gj\xF8r dette for \xE5\
  \ h\xE5ndtere\u2026"
lastmod: '2024-03-11T00:14:14.317939-06:00'
model: gpt-4-1106-preview
summary: "Sammenligning av to datoer handler om \xE5 finne ut om de er like, eller\
  \ hvilken som kommer f\xF8r eller etter den andre. Utviklere gj\xF8r dette for \xE5\
  \ h\xE5ndtere\u2026"
title: Sammenlikning av to datoer
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenligning av to datoer handler om å finne ut om de er like, eller hvilken som kommer før eller etter den andre. Utviklere gjør dette for å håndtere frister, events eller datafiltrering i apper.

## Hvordan:
```kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 4, 15)
    val date2 = LocalDate.of(2023, 5, 10)

    println("Er datoene like? ${date1.isEqual(date2)}")
    println("Kommer date1 før date2? ${date1.isBefore(date2)}")
    println("Kommer date1 etter date2? ${date1.isAfter(date2)}")
}

// Output:
// Er datoene like? false
// Kommer date1 før date2? true
// Kommer date1 etter date2? false
```

## Dypdykk
Tidligere brukte vi `java.util.Date` og `java.util.Calendar` for å håndtere datoer i Java, men de hadde flere utfordringer, inkludert mutabilitet og dårlig design. Kotlin er bygget på JVM og bruker `java.time`-pakken introdusert i Java 8 for datooperasjoner. Denne pakken er både enklere å bruke og mer robust.

Det finnes andre måter å sammenligne datoer på, som bruk av tidsstempel eller eksterne biblioteker som Joda-Time. Men, med `java.time`, spesielt `LocalDate`, `LocalTime` og `LocalDateTime`, har man sterk støtte rett ut av boksen.

Implementasjonen bak disse sammenligningsmetodene bruker enkel aritmetikk på lagrede datoattributter. `isEqual()`, `isBefore()` og `isAfter()` er intuitive og selvforklarende metoder som gjør det enkelt å jobbe med datoer.

## Se Også
- Offisiell Kotlin-dokumentasjon om dato- og tidsbiblioteker: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/)
- Java 8 `java.time`-pakke: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Joda-Time biblioteket som alternativ: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
