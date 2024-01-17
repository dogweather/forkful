---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Kotlin: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Beräkning av ett datum i framtiden eller det förflutna är en vanlig uppgift för programmerare. Det är oftast när vi vill hantera datum och tider som ligger utanför det aktuella datumet. Det kan vara att planera ett schema, skapa en kalender eller hantera tidsbegränsade erbjudanden.

## Så här gör du:

Enklaste sättet att beräkna ett datum i framtiden eller det förflutna är att använda inbyggda klassen `DateTime` från Kotlin's Standard Library. Här är ett exempel där vi beräknar datumet för exakt en månad framåt från det aktuella datumet:

```Kotlin
val currentDate = DateTime.now()
val futureDate = currentDate.plusMonths(1)
println("Datumet om en månad är: $futureDate")
```

Detta kommer att producera output som: `Datumet om en månad är: 2021-11-15T16:06:31.356Z`

Om du vill beräkna ett datum i det förflutna, kan du använda funktionen `minus` istället. Till exempel för att beräkna datumet för exakt ett år sedan:

```Kotlin
val currentDate = DateTime.now()
val pastDate = currentDate.minusYears(1)
println("Datumet för ett år sedan var: $pastDate")
```

Detta kommer att producera output som: `Datumet för ett år sedan var: 2020-10-15T16:07:30.465Z`

## Djupdykning:

I det förflutna har beräkning av datum varit en komplicerad uppgift, särskilt när man tar hänsyn till skottår och olika tidszoner. Men tack vare utvecklingen av moderna språk som Kotlin, har det blivit mycket enklare att hantera datum och tider.

Det finns också alternativ till Kotlin's `DateTime` klassen som används för att beräkna datum. En av dem är `LocalDateTime` som hanterar datum och tider utan att ta hänsyn till tidszoner.

När man beräknar datum i det förflutna bör man också ta hänsyn till olika tidszoner, eftersom datumet kan variera beroende på var i världen man beräknar det. Om man behöver hantera datum internationellt kan man använda klassen `ZonedDateTime` som hanterar både datum och tider samt tidszoner.

## Se även:

- Kotlin Standard Library's dokumentation för `DateTime`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date-time/
- Joda-Time, ett populärt Java-bibliotek för datum och tider: https://www.joda.org/joda-time/
- Java's `LocalDate` och `ZonedDateTime` klasser för hantering av datum: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html, https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html