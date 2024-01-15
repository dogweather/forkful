---
title:                "Beräkning av ett datum i framtiden eller det förflutna."
html_title:           "Kotlin: Beräkning av ett datum i framtiden eller det förflutna."
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

### Varför

Kotlin är ett modernt programmeringsspråk som är väldigt populärt i dagens teknikvärld. Med sin enkla syntax och mångsidighet kan det användas för en rad olika uppgifter, inklusive att beräkna datum i framtiden eller det förflutna. Detta kan vara användbart i många olika situationer, till exempel för att planera möten eller resor, eller för att hålla koll på deadlines.

### Så här gör du

För att beräkna ett datum i framtiden eller det förflutna i Kotlin, kan du följa dessa steg:

1. Skapa en variabel för det aktuella datumet genom att använda klassen LocalDate.

```Kotlin
val currentDate = LocalDate.now()
```

2. Använd sedan klassen LocalDate igen för att skapa en variabel för det datumet du vill beräkna från.

```Kotlin
val dateToCalculate = LocalDate.of(2021, Month.FEBRUARY, 15)
```

3. För att beräkna datumet i framtiden, kan du använda metoden plusDays() eller plusMonths() i kombination med variabeln för det aktuella datumet.

```Kotlin
val futureDate = currentDate.plusDays(7) //lägger till 7 dagar till det aktuella datumet
val futureDate = currentDate.plusMonths(1) //lägger till 1 månad till det aktuella datumet
```

4. Om du istället vill beräkna ett datum i det förflutna, kan du använda metoden minusDays() eller minusMonths() på samma sätt.

```Kotlin
val pastDate = currentDate.minusDays(7) //drar av 7 dagar från det aktuella datumet
val pastDate = currentDate.minusMonths(1) //drar av 1 månad från det aktuella datumet
```

### Djupdykning

Kotlin har inbyggda klasser och metoder för att hantera datum och tid, vilket gör det enkelt att beräkna datum i framtiden eller det förflutna. Det är också möjligt att använda andra metoder för att göra mer exakta beräkningar, till exempel plusYears(), plusWeeks() eller minusHours().

Utöver detta kan du också utföra jämförelser mellan olika datum genom att använda metoden compareTo(). Detta kan vara användbart för att se till exempel om ett datum ligger före eller efter ett annat.

### Se även

- [Kotlin dokumentation](https://kotlinlang.org/docs/home.html)
- [Java Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)