---
title:                "Att hämta aktuellt datum"
html_title:           "Kotlin: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Innan vi dyker in i hur man hämtar det aktuella datumet i Kotlin, låt oss först prata om varför det är viktigt. Att kunna hämta det aktuella datumet är en viktig del av många programmeringsuppgifter. Det kan hjälpa oss att hålla reda på när en viss händelse inträffade, spela in tidsstämplingar för transaktioner eller enkelt visa det aktuella datumet för användaren i ett gränssnitt.

## Hur man hämtar det aktuella datumet
Att hämta det aktuella datumet i Kotlin är enkelt och kan göras på flera olika sätt. Ett sätt är att använda klassen `java.util.Calendar` och dess `getInstance()` metod för att skapa en instans av kalendern.

```Kotlin
val currentDate = Calendar.getInstance()
println(currentDate.time) // output: Sat Apr 03 14:19:46 EDT 2021
```

En annan metod är att använda klassen `java.time.LocalDate` från Javas nya tids-API.

```Kotlin
val currentDate = LocalDate.now()
println(currentDate) // output: 2021-04-03
```

## Djupdykning
För de som vill ha lite djupare kunskap om hur man hämtar det aktuella datumet i Kotlin, låt oss titta på det första exempelkodet igen och bryta ner det steg för steg.

Först skapar vi en instans av `Calendar` klassen med hjälp av `getInstance()` metoden. Detta ger oss en kalenderinstans med aktuellt datum och tid som standard. Vi kan också ställa in ett specifikt datum och tid på kalendern om det behövs.

Sedan använder vi `time` metoden för att få ett `java.util.Date` objekt som representerar det aktuella datumet och tiden. Sedan skriver vi ut det till konsolen med `println()`.

Om vi istället använder `LocalDate` måste vi förstå den nya tids-API:et som introducerades i Java 8. Denna API använder den ISO-datum standarden och har förbättrad funktionalitet jämfört med den äldre `Calendar` klassen.

## Se även
Om du vill lära dig mer om att hämta datum i Kotlin, här är några användbara resurser:

- [Dokumentation för Java Calendar klassen](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Dokumentation för Java 8 tids-API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Kotlin Standard Library - Datum och tid](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/index.html#date-time-apis)