---
title:    "Kotlin: Beräkning av en datum i framtiden eller förflutna"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna ett datum i framtiden eller förfluten tid är en viktig färdighet inom programmering. Det kan hjälpa till att lösa problem och effektivisera arbetsflöden.

## Hur man gör det
Det finns flera olika sätt att beräkna ett datum i Kotlin beroende på vilka förutsättningar och vilken precision som behövs. Här nedanför visas några exempel på hur man kan göra det.

```Kotlin
// Beräkna ett datum x antal dagar framåt
val idag = LocalDate.now()
val antalDagar = 10
val framtidDatum = idag.plusDays(antalDagar)
println("Datum om $antalDagar dagar: $framtidDatum")
// Output: Datum om 10 dagar: 2021-11-06

// Beräkna ett datum x antal månader bakåt
val idag = LocalDate.now()
val antalMånader = 3
val förflutenDatum = idag.minusMonths(antalMånader)
println("Datum för $antalMånader månader sedan: $förflutenDatum")
// Output: Datum för 3 månader sedan: 2021-05-06

// Beräkna ett datum utifrån ett specifikt datum
val början = LocalDate.of(2020, 1, 1)
val slut = LocalDate.of(2021, 1, 1)
val antalDagar = ChronoUnit.DAYS.between(början, slut)
println("Antal dagar mellan $början och $slut är: $antalDagar")
// Output: Antal dagar mellan 2020-01-01 och 2021-01-01 är: 366
```

## Djupdykning
För att kunna beräkna ett datum i framtiden eller förfluten tid i Kotlin så behöver man förstå hur datatyperna LocalDate och ChronoUnit fungerar. LocalDate är en klass som representerar ett datum och har metoder för att manipulera det datumet. ChronoUnit är en enum som innehåller olika enheter av tid som kan användas för att beräkna skillnaden mellan två datum.

## Se även
- Dokumentation för LocalDate och ChronoUnit: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/
- Java 8 Time API: https://www.baeldung.com/java-8-date-time-intro