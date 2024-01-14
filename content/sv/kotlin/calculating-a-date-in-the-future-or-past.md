---
title:                "Kotlin: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att beräkna ett datum i framtiden eller det förflutna kan vara användbart för att planera evenemang, övervaka deadlines eller bara för att hålla koll på tiden. Med hjälp av Kotlin-programmeringsspråket kan du enkelt implementera denna funktionalitet i dina projekt.

## Så här gör du

Först och främst behöver du importera klassen `LocalDate` från Kotlin standardbiblioteket. Sedan kan du använda funktionen `plusDays` eller `minusDays` för att lägga till eller dra bort önskat antal dagar från det aktuella datumet.

```Kotlin
val today = LocalDate.now()
val futureDate = today.plusDays(10)
val pastDate = today.minusDays(7)

println("Idag: $today")
println("Datum i framtiden: $futureDate")
println("Datum i förflutnan: $pastDate")
```

Output:

```
Idag: 2021-09-15
Datum i framtiden: 2021-09-25
Datum i förflutnan: 2021-09-08
```

Det är också möjligt att beräkna datum baserat på andra tidsenheter som veckor, månader eller till och med år.

```Kotlin
val futureWeek = today.plusWeeks(3)
val futureMonth = today.plusMonths(4)
val futureYear = today.plusYears(2)

println("Datum om 3 veckor: $futureWeek")
println("Datum om 4 månader: $futureMonth")
println("Datum om 2 år: $futureYear")
```

Output:

```
Datum om 3 veckor: 2021-10-06
Datum om 4 månader: 2022-01-15
Datum om 2 år: 2023-09-15
```

Det är också möjligt att utgå från ett specifikt datum istället för det aktuella datumet. Till exempel, om du vill beräkna ett datum som ligger 30 år tillbaka i tiden från och med den 1 januari 2021, kan du göra det på följande sätt:

```Kotlin
val specificDate = LocalDate.of(2021, Month.JANUARY, 1)
val pastDate = specificDate.minusYears(30)

println("Specifikt datum: $specificDate")
println("Datum för 30 år sedan: $pastDate")
```

Output:

```
Specifikt datum: 2021-01-01
Datum för 30 år sedan: 1991-01-01
```

## Djupdykning

För att beräkna en exakt tidsperiod i år, måste du också överväga skottår, vilket har en extra dag i februari varje fyra år. För att hantera detta behöver du utöver funktionerna för att lägga till eller dra bort ett visst antal dagar, även använda funktionen `plusYears` eller `minusYears` för att justera året om det är ett skottår.

```Kotlin
val specificDate = LocalDate.of(2020, Month.FEBRUARY, 29)
val fourYearsLater = specificDate.plusYears(4)
val fiveYearsLater = specificDate.plusYears(5)

println("Startdatum: $specificDate")
println("Datum efter fyra år: $fourYearsLater")
println("Datum efter fem år: $fiveYearsLater")
```

Output:

```
Startdatum: 2020-02-29
Datum efter fyra år: 2024-02-29
Datum efter fem år: 2025-02-28
```

Observera att efter fem år så har datumen justerats till nästa dag, eftersom år 2025 inte är ett skottår.

## Se också

- [Kotlin standardbiblioteket dokumentation för LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/index.html)
- [Java-tutorial för LocalDate](https://docs.oracle.com/javase/tutorial/datetime/iso/datetime.html)