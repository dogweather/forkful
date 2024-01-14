---
title:                "Java: Beräkna ett datum i framtiden eller i det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller i det förflutna"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller det förflutna kan vara väldigt användbart i många olika situationer. Till exempel kan det vara till hjälp för att planera framtida händelser eller för att spåra händelser som redan har inträffat.

## Hur man gör det

För att beräkna ett datum i framtiden eller förflutna behöver man använda sig av klassen `LocalDate` från Java API. Det finns flera olika metoder som kan användas för att beräkna ett datum, beroende på vilka parametrar man vill använda.

Här är ett exempel på hur man beräknar ett datum 10 dagar framåt från nuvarande datum:

```Java
LocalDate today = LocalDate.now();// Nuvarande datum

LocalDate futureDate = today.plusDays(10); // Beräknar datumet 10 dagar framåt

System.out.println(futureDate); // Skriver ut resultatet: "2020-09-16"
```

För att beräkna ett datum i det förflutna kan man använda `minusDays()` istället för `plusDays()`.

## Djupdykning

Metoderna `plusDays()` och `minusDays()` är bara två av många som finns tillgängliga för att beräkna datum. Det finns även metoder för att beräkna datum baserat på månader, år och veckor.

En annan användbar funktion är `with()` som kan användas för att ändra ett specifikt datumattribut, exempelvis att byta ut månaden till en annan månad.

## Se även

- [Java API - LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- [Oracle Java Tutorials - Dates and Time](https://docs.oracle.com/javase/tutorial/datetime/index.html)
- [Java Date and Time API Tutorial](https://www.baeldung.com/java-8-date-time-intro)